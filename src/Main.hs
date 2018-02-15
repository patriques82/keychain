{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Data.List                  (find, intercalate, init)
import           Data.Maybe                 (catMaybes)
import           Control.Exception          (bracket_)
import           Control.Monad              (mapM_)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
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
import qualified Data.Yaml                  as Y
import qualified Data.HashMap.Strict        as HM (toList, lookup)

-- Types
type Error = String
type EncryptedFilePath = FilePath
type PasswordFilePath = FilePath
type Password = String
type Site = String
type Key = String
type User = String

data SiteDetails = SiteDetails {
  name :: T.Text,
  siteKey :: Maybe T.Text,
  user :: Maybe T.Text
} deriving (Eq, Show)

-- | IO with error checking
newtype IOException a = IOException {
  runIOException :: ExceptT Error IO a
} deriving (Functor, Applicative, Monad, MonadIO)

newtype PasswordApp = App {
  runApp :: Password -> IOException ()
}

main :: IO ()
main = getArgs >>= run . parse

parse :: [String] -> PasswordApp
parse ["setup", e, "-p", p] = setupOrigin e p
parse ["setup", e]          = setupRemote e
parse ["key", s]            = key s
parse ["list"]              = list
parse ["-h"]                = undefined
parse _                     = undefined

run :: PasswordApp -> IO ()
run app = getPassword >>= runWithPassword >>= either putStrLn return
  where runWithPassword = runExceptT . runIOException . runApp app

-- | Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> PasswordApp
setupOrigin encryptedPath passwordPath = App $ \p -> do
  encrypted <- encrypt passwordPath p
  write encryptedPath encrypted
  c <- configPath
  write c (pack encryptedPath)

-- | Takes filepath to location of existing encrypted file and stores path to encrypted file in lockfile (config).
setupRemote :: EncryptedFilePath -> PasswordApp
setupRemote encryptedPath = App $ \p -> do
  assertExist encryptedPath
  s <- decrypt encryptedPath p 
  case parseSiteDetails s of
    Nothing -> throw "Incorrect password"
    _ -> do 
      c <- configPath
      write c (pack encryptedPath)

-- | Takes sitename and adds the key for that site to the clipboard.
key :: Site -> PasswordApp
key s = mkPasswordApp $ \xs -> 
  case findKey s xs of
    Nothing -> putStrLn "Key not found"
    Just k -> setClipboard (T.unpack k) 

-- | Lists name of all site details in encrypted file
list :: PasswordApp
list = mkPasswordApp $ mapM_ TIO.putStrLn . catMaybes . map siteKey

-- Parsing

parseSiteDetails :: BS.ByteString -> Maybe [SiteDetails]
parseSiteDetails bs = Y.decode bs >>= Y.parseMaybe siteDetailsParser

siteDetailsParser :: Y.Value -> Y.Parser [SiteDetails]
siteDetailsParser =
  Y.withObject "details" $ \o ->
    for (HM.toList o) $ \(siteName, details) -> do
      details' <- Y.parseJSON details
      return $ SiteDetails siteName (HM.lookup "password" details') (HM.lookup "username" details')

-- IOExeptions

mkPasswordApp :: ([SiteDetails] -> IO ()) -> PasswordApp 
mkPasswordApp f = App $ \p -> do
  e <- encryptedFilePath
  c <- decrypt e p
  case parseSiteDetails c of
    Nothing -> throw "Something went wrong with parsing"
    Just xs -> liftIO (f xs)

encrypt :: FilePath -> Password -> IOException BS.ByteString
encrypt f p = do
  assertExist f
  liftIO $ BS.readFile f >>= CBC.encrypt (pack p)

decrypt :: FilePath -> Password -> IOException BS.ByteString
decrypt f p = do
  assertExist f
  liftIO $ BS.readFile f >>= CBC.decrypt (pack p)

write :: FilePath -> BS.ByteString -> IOException ()
write f c = do
  assertWritable f
  liftIO $ BS.writeFile f c

encryptedFilePath :: IOException FilePath
encryptedFilePath = do
  c <- configPath
  e <- liftIO $ readFile c
  case lines e of
    (e':[]) -> assertExist e' >> return e'
    _ -> throw "Empty config"

configPath :: IOException FilePath
configPath = flip (</>) ".lockfile" <$> liftIO getHomeDirectory

assertWritable :: FilePath -> IOException ()
assertWritable path = 
  if dir == path
    then throw "Filepath is a directory"
    else do
      exists <- liftIO $ doesDirectoryExist dir
      if exists 
        then return ()
        else throw $ "Directory " ++ dir ++ " does not exist"
    where dir = directory path

assertExist :: FilePath -> IOException ()
assertExist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return ()
    else throw ("File " ++ path ++ " does not exist")

throw :: Error -> IOException a
throw s = IOException $ throwE s

-- helper functions

directory :: FilePath -> FilePath
directory = intercalate "/" . init . split '/'

getPassword :: IO Password
getPassword = do
  putStr "Password: "
  hFlush stdout
  password <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine 
  putChar '\n'
  return $ padR (17 - length password) '0' password

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

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = find ((==) (T.pack s) . name) xs >>= siteKey
