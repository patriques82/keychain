{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Data.List                  (find, intercalate)
import           Control.Exception          (bracket_)
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Crypto.Simple.CBC          as CBC (decrypt, encrypt)
import qualified Data.ByteString            as BS (ByteString, readFile,
                                                   writeFile)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.Text                  as T (Text, pack)
import           Data.Text.Encoding         (decodeUtf8)
--import qualified Data.Text.IO               as TIO (putStrLn)
import           System.IO                  (hFlush, hGetEcho, hSetEcho, stdout, stdin)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment         (getArgs)
import           System.FilePath.Posix      ((</>))
import           System.Hclip               (setClipboard)
import           Data.Traversable           (for)
import qualified Data.Yaml                  as Y
import qualified Data.HashMap.Strict        as HM

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

type App = Password -> IOException ()

delim = ':'
lockfile = ".lockfile"

-- Main

main :: IO ()
main = getArgs >>= exec

exec :: [String] -> IO ()
exec ["setup", e, "-p", p] = run $ setupOrigin e p
exec ["setup", e]          = undefined  -- run $ setupRemote e
exec ["key", s]            = run $ key s
exec ["list"]              = run list
exec ["-h"]                = undefined
exec _                     = undefined

run :: App -> IO ()
run app = getPassword >>= runWithPassword >>= either putStrLn return
  where runWithPassword = runExceptT . runIOException . app

-- command semantics

-- |Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores paths to both in lockfile (config).
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> App
setupOrigin encryptedPath passwordPath p = do
  passwordPath' <- exist passwordPath
  encrypted <- liftIO $ encrypt passwordPath' p
  liftIO $ BS.writeFile encryptedPath encrypted
  c <- configPath
  liftIO $ writeFile c configContent
  where configContent = intercalate [delim] [encryptedPath, passwordPath]

-- |Takes sitename and adds the key for that site to the clipboard.
key :: Site -> App
key s p = do
  f <- encryptedFile
  f' <- liftIO $ CBC.decrypt (pack p) f
  case parse f' of
    Nothing -> throw "Something went wrong with parsing"
    Just xs -> liftIO $ print (findKey s xs) -- Maybe Text
  -- liftIO $ setClipboard (unpack f')

-- |List all site names available.
list :: App
list p = do
  f <- encryptedFile
  f' <- liftIO $ CBC.decrypt (pack p) f
  case parse f' of
    Nothing -> throw "Something went wrong with parsing"
    Just xs -> liftIO $ forM_ xs (print . siteKey) 

encrypt :: FilePath -> Password -> IO BS.ByteString
encrypt fp password = BS.readFile fp >>= CBC.encrypt (pack password)

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
encryptedFile = configFilePaths >>= liftIO . BS.readFile . head

configFilePaths :: IOException [FilePath]
configFilePaths = do
  cf <- configPath'
  s <- liftIO $ readFile cf
  if s /= "" then mapM exist (split s) else throw "Empty config"

decryptFile :: Password -> BS.ByteString -> IOException T.Text
decryptFile p f = do
  let p' = pack p
  f' <- liftIO $ CBC.decrypt p' f
  return $ decodeUtf8 f'

configPath' :: IOException FilePath
configPath' = configPath >>= exist

configPath :: IOException FilePath
configPath = flip (</>) lockfile <$> liftIO getHomeDirectory

exist :: FilePath -> IOException FilePath
exist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return path
    else throw $ "No such file: " ++ path

throw :: Error -> IOException a
throw s = IOException $ throwE s

-- helper functions

concatFilePaths :: FilePath -> FilePath -> String
concatFilePaths f p = f ++ (delim:p)

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

split :: String -> [String]
split s =
  case dropWhile (== delim) s of
    "" -> []
    s' -> w : split s''
      where (w, s'') = break (== delim) s'

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = find ((==) (T.pack s) . name) xs >>= siteKey
