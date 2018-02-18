{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (pack)
import           Data.List                  (find)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Yaml                  ((.=), (.:), (.:?))
import qualified Data.Yaml                  as Y 
import           Control.Exception          (bracket_)
import           Control.Monad              (mapM_)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import qualified Crypto.Simple.CBC          as CBC
import           System.Directory           (doesFileExist, doesDirectoryExist, getHomeDirectory)
import           System.Environment         (getArgs)
import           System.FilePath.Posix      ((</>), takeDirectory, isValid)
import           System.Hclip               (setClipboard)
import           System.IO                  (hFlush, hSetEcho, stdout, stdin)

type Error = String
type EncryptedFilePath = FilePath
type PasswordFilePath = FilePath
type Password = String
type Site = String
type Key = String
type User = String

data SiteDetails = SiteDetails {
  name :: T.Text,
  user :: Maybe T.Text,
  key :: Maybe T.Text
} deriving (Eq, Show)

instance Y.ToJSON SiteDetails where
  toJSON SiteDetails{..} = Y.object [
    "name" .= name,
    "user" .= user,
    "key" .= key ]

instance Y.FromJSON SiteDetails where 
  parseJSON = Y.withObject "details" $ \v -> do
    name <- v .: "name"
    user <- v .:? "user"
    key <- v .:? "key"
    return SiteDetails{..}

newtype App a = App {
  runApp :: ReaderT Password (ExceptT Error IO) a
} deriving (Functor, Applicative, Monad, MonadIO)

type PasswordApp = App () 

main :: IO ()
main = getArgs >>= keychain

keychain :: [String] -> IO ()
keychain = run . parse

parse :: [String] -> PasswordApp
parse ["setup", e, "-p", p] = setupOrigin e p
parse ["setup", e]          = setupRemote e
parse ["user", s]           = siteUser s
parse ["key", s]            = siteKey s
parse ["list"]              = list
parse ["sync", p]           = sync p
parse ["-h"]                = undefined
parse _                     = undefined

run :: PasswordApp -> IO ()
run app = getPassword >>= runWithPassword >>= either printError finish
  where runWithPassword = runExceptT . runReaderT (runApp app)
        printError = putStrLn
        finish = return

-- | Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> PasswordApp
setupOrigin encryptedPath passwordPath = do
  encrypted <- encrypt passwordPath
  write encrypted encryptedPath 
  c <- configPath
  write (pack encryptedPath) c

-- | Takes filepath to location of existing encrypted file and stores path to encrypted file in lockfile (config).
setupRemote :: EncryptedFilePath -> PasswordApp
setupRemote e = passwordApp $ \_ -> configPath >>= write (pack e)

-- | Adds the key for given site to the clipboard.
siteKey :: Site -> PasswordApp
siteKey s = passwordApp $ \xs -> 
  case findKey s xs of
    Nothing -> liftIO $ putStrLn $ "Site " ++ s ++ " not found"
    Just k -> liftIO $ setClipboard (T.unpack k) 

-- | Adds the username for given site to the clipboard.
siteUser :: Site -> PasswordApp
siteUser s = passwordApp $ \xs -> 
  case findUser s xs of
    Nothing -> liftIO $ putStrLn $ "Site " ++ s ++ " not found"
    Just u -> liftIO $ setClipboard (T.unpack u) 

-- | Lists name of all sites in encrypted file
list :: PasswordApp
list = passwordApp $ liftIO . mapM_ (TIO.putStrLn . name)

-- | Unencrypts encryptedfile and stores contents at given filepath
sync :: PasswordFilePath -> PasswordApp
sync p = passwordApp $ \xs -> do
  assertWritable p
  liftIO $ Y.encodeFile p xs

-- App Combinators

passwordApp :: ([SiteDetails] -> App ()) -> PasswordApp
passwordApp f = do
  e <- encryptedFilePath
  s <- decrypt e
  case Y.decode s of
    Nothing -> throw "Incorrect password"
    Just xs -> f xs

encrypt :: FilePath -> App BS.ByteString
encrypt f = do
  assertExist f
  p <- password
  liftIO $ BS.readFile f >>= CBC.encrypt (pack p)

decrypt :: FilePath -> App BS.ByteString
decrypt f = do
  assertExist f
  p <- password
  liftIO $ BS.readFile f >>= CBC.decrypt (pack p)

write :: BS.ByteString -> FilePath -> App ()
write b f = do
  assertWritable f
  liftIO $ BS.writeFile f b

encryptedFilePath :: App FilePath
encryptedFilePath = do
  c <- configPath
  e <- liftIO $ readFile c
  case lines e of
    (e':[]) -> return e'
    _ -> throw "Config does not contain file path"

configPath :: App FilePath
configPath = flip (</>) ".lockfile" <$> liftIO getHomeDirectory

assertWritable :: FilePath -> App ()
assertWritable path = do
  if isValid path
    then do 
      let dir = takeDirectory path
      exists <- liftIO $ doesDirectoryExist dir
      if exists 
        then return ()
        else throw $ "Directory " ++ dir ++ " does not exist"
    else throw $ "Path " ++ path ++ " is not valid"

assertExist :: FilePath -> App ()
assertExist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return ()
    else throw $ "File " ++ path ++ " does not exist"

throw :: Error -> App a
throw = App . lift . throwE

password :: App Password
password = App $ ask

-- helper functions

getPassword :: IO Password
getPassword = do
  putStr "Password: "
  hFlush stdout
  p <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine 
  putChar '\n'
  return $ padR (17 - length p) '0' p

-- appends 0:s until n characters have been reached
padR :: Int -> Char -> String -> String
padR n c cs
  | n > 0 = cs ++ replicate n c
  | otherwise = cs

split :: Char -> String -> [String]
split d s =
  case dropWhile (== d) s of
    "" -> []
    s' -> w : split d s''
      where (w, s'') = break (== d) s'

findSite :: Site -> [SiteDetails] -> Maybe SiteDetails
findSite s xs = find ((==) (T.pack s) . name) xs

findUser :: Site -> [SiteDetails] -> Maybe T.Text
findUser s xs = findSite s xs >>= user

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = findSite s xs >>= key