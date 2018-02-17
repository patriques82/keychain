{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Data.List                  (find, intercalate, init)
import           Control.Exception          (bracket_)
import           Control.Monad              (mapM_)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import qualified Crypto.Simple.CBC          as CBC (decrypt, encrypt)
import qualified Data.ByteString            as BS (ByteString, readFile, writeFile)
import           Data.ByteString.Char8      (pack)
import qualified Data.Text                  as T (Text, pack, unpack)
import qualified Data.Text.IO               as TIO (putStrLn)
import           System.IO                  (hFlush, hSetEcho, stdout, stdin)
import           System.Directory           (doesFileExist, doesDirectoryExist, getHomeDirectory)
import           System.Environment         (getArgs)
import           System.FilePath.Posix      ((</>))
import           System.Hclip               (setClipboard)
import           Data.Traversable           (for)
import qualified Data.Yaml                  as Y (Value, Parser, decode, parseMaybe, withObject, parseJSON)
import qualified Data.HashMap.Strict        as HM (toList, lookup)

type Error = String
type EncryptedFilePath = FilePath
type PasswordFilePath = FilePath
type Password = String
type Site = String
type Key = String
type User = String

data SiteDetails = SiteDetails {
  name :: T.Text,
  key :: Maybe T.Text,
  user :: Maybe T.Text
} deriving (Eq, Show)

newtype App a = App {
  runApp :: ReaderT Password (ExceptT Error IO) a
} deriving (Functor, Applicative, Monad, MonadIO)

type PasswordApp = App () 

main :: IO ()
main = getArgs >>= run . parse

parse :: [String] -> PasswordApp
parse ["setup", e, "-p", p] = setupOrigin e p
parse ["setup", e]          = setupRemote e
parse ["user", s]           = siteUser s
parse ["key", s]            = siteKey s
parse ["list"]              = list
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
  write encryptedPath encrypted
  c <- configPath
  write c (pack encryptedPath)

-- | Takes filepath to location of existing encrypted file and stores path to encrypted file in lockfile (config).
setupRemote :: EncryptedFilePath -> PasswordApp
setupRemote encryptedPath = do
  assertExist encryptedPath
  s <- decrypt encryptedPath
  case parseSiteDetails s of
    Nothing -> throw "Incorrect password"
    _ -> do 
      c <- configPath
      write c (pack encryptedPath)

-- | Adds the key for given site to the clipboard.
siteKey :: Site -> PasswordApp
siteKey s = mkApp $ \xs -> 
  case findKey s xs of
    Nothing -> putStrLn $ "Site " ++ s ++ " not found"
    Just k -> setClipboard (T.unpack k) 

-- | Adds the siteUser for given site to the clipboard.
siteUser :: Site -> PasswordApp
siteUser s = mkApp $ \xs -> 
  case findUser s xs of
    Nothing -> putStrLn $ "Site " ++ s ++ " not found"
    Just u -> setClipboard (T.unpack u) 

-- | Lists name of all site details in encrypted file
list :: PasswordApp
list = mkApp $ mapM_ (TIO.putStrLn . name)

-- App Combinators

mkApp :: ([SiteDetails] -> IO ()) -> App ()
mkApp f = do
  e <- encryptedFilePath
  s <- decrypt e
  case parseSiteDetails s of
    Nothing -> throw "Incorrect password"
    Just xs -> liftIO (f xs)

password :: App Password
password = App $ ask

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

write :: FilePath -> BS.ByteString -> App ()
write f c = do
  assertWritable f
  liftIO $ BS.writeFile f c

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
  let dir = directory path
  exists <- liftIO $ doesDirectoryExist dir
  if exists 
    then return ()
    else throw $ "Directory " ++ dir ++ " does not exist"

assertExist :: FilePath -> App ()
assertExist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return ()
    else throw $ "File " ++ path ++ " does not exist"

throw :: Error -> App a
throw s = App $ ReaderT $ \_ -> throwE s

-- Parsing

parseSiteDetails :: BS.ByteString -> Maybe [SiteDetails]
parseSiteDetails bs = Y.decode bs >>= Y.parseMaybe siteDetailsParser

siteDetailsParser :: Y.Value -> Y.Parser [SiteDetails]
siteDetailsParser =
  Y.withObject "details" $ \o ->
    for (HM.toList o) $ \(sitename, details) -> do
      details' <- Y.parseJSON details
      return $ SiteDetails sitename (HM.lookup "password" details') (HM.lookup "siteUsername" details')

-- helper functions

directory :: FilePath -> FilePath
directory = intercalate "/" . init . split '/'

getPassword :: IO Password
getPassword = do
  putStr "Password: "
  hFlush stdout
  p <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine 
  putChar '\n'
  return $ padR (17 - length p) '0' p

-- appends 0:s until 17 characters have been reached
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
findUser s xs = user =<< findSite s xs

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = key =<< findSite s xs