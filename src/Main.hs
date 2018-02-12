{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Data.List                  (find, intercalate, init)
import           Control.Exception          (bracket_)
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Crypto.Simple.CBC          as CBC (decrypt, encrypt)
import qualified Data.ByteString            as BS (ByteString, readFile, writeFile)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.Text                  as T (Text, pack)
import           Data.Text.Encoding         (decodeUtf8)
--import qualified Data.Text.IO               as TIO (putStrLn)
import           System.IO                  (hFlush, hGetEcho, hSetEcho, stdout, stdin)
import           System.Directory           (doesFileExist, doesDirectoryExist, getHomeDirectory, setPermissions)
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

newtype IOException a = IOException {
  runIOException :: ExceptT Error IO a  -- IO (Either Error a)
} deriving (Functor, Applicative, Monad, MonadIO)

newtype App a b = App {
  runApp :: a -> IOException b
}

type PasswordApp = App Password ()

main :: IO ()
main = getArgs >>= exec

exec :: [String] -> IO ()
exec ["setup", e, "-p", p] = run $ setupOrigin e p
exec ["setup", e]          = undefined  -- run $ setupRemote e
exec ["key", s]            = run $ key s
exec ["list"]              = run list
exec ["-h"]                = undefined
exec _                     = undefined

run :: PasswordApp -> IO ()
run app = getPassword >>= runWithPassword >>= either putStrLn return
  where runWithPassword = runExceptT . runIOException . runApp app

-- |Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> PasswordApp
setupOrigin encryptedPath passwordPath = App $ \p -> do
  encryptedPath' <- writable encryptedPath
  passwordPath' <- exist passwordPath
  encrypted <- liftIO $ encrypt passwordPath' p
  liftIO $ BS.writeFile encryptedPath' encrypted
  c <- configPath
  liftIO $ writeFile c encryptedPath'

setupRemote :: EncryptedFilePath -> IOException ()
setupRemote encryptedPath = do
  encryptedPath' <- exist encryptedPath
  c <- configPath
  liftIO $ writeFile c encryptedPath'

-- |Takes sitename and adds the key for that site to the clipboard.
key :: Site -> PasswordApp
key s = App $ \p -> do
  f <- encryptedFile
  f' <- decrypt f p
  case parse f' of
    Nothing -> throw "Something went wrong with parsing"
    Just xs -> liftIO $ print (findKey s xs) -- Maybe Text
  -- liftIO $ setClipboard (unpack f')

-- |List all site names available.
list :: PasswordApp
list = App $ \p -> do
  f <- encryptedFile
  f' <- decrypt f p
  case parse f' of
    Nothing -> throw "Something went wrong with parsing"
    Just xs -> liftIO $ forM_ xs (print . siteKey) 

encrypt :: FilePath -> Password -> IO BS.ByteString
encrypt f password = BS.readFile f >>= CBC.encrypt (pack password)

decrypt :: BS.ByteString -> Password -> IOException BS.ByteString
decrypt f p = liftIO $ CBC.decrypt (pack p) f

-- Parsing

parse :: BS.ByteString -> Maybe [SiteDetails]
parse bs = Y.decode bs >>= Y.parseMaybe parseSiteDetails

parseSiteDetails :: Y.Value -> Y.Parser [SiteDetails]
parseSiteDetails =
  Y.withObject "details" $ \o ->
    for (HM.toList o) $ \(siteName, details) -> do
      details' <- Y.parseJSON details
      return $ SiteDetails siteName (HM.lookup "password" details') (HM.lookup "username" details')

-- IOExeptions

encryptedFile :: IOException BS.ByteString
encryptedFile = configFilePaths >>= liftIO . BS.readFile

configFilePaths :: IOException FilePath
configFilePaths = do
  cf <- configPath >>= exist
  s <- liftIO $ readFile cf
  if s /= ""
    then exist s
    else throw "Empty config"

configPath :: IOException FilePath
configPath = flip (</>) ".lockfile" <$> liftIO getHomeDirectory

writable :: FilePath -> IOException FilePath
writable path = 
  if dir == path
    then throw "Filepath is a directory"
    else do
      exists <- liftIO $ doesDirectoryExist dir
      if exists 
        then return path
        else throw $ "Directory " ++ dir ++ " does not exist"
    where dir = directory path

exist :: FilePath -> IOException FilePath
exist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return path
    else throw $ "File " ++ path ++ " does not exist"

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
