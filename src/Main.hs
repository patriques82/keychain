{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Data.List                  (find, intercalate)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Crypto.Simple.CBC          as CBC (decrypt, encrypt)
import qualified Data.ByteString            as BS (ByteString, readFile,
                                                   writeFile)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.Text                  as T (Text, pack)
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.IO               as TIO (putStrLn)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment         (getArgs)
import           System.FilePath.Posix      ((</>))
import           System.Hclip               (setClipboard)
import           Data.Traversable (for)
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

newtype App a = App {
  runApp :: ExceptT Error IO a  -- IO (Either Error a)
} deriving (Functor, Applicative, Monad, MonadIO)

delim = ':'
lockfile = ".lockfile"

-- Main

main :: IO ()
main = getArgs >>= exec

exec :: [String] -> IO ()
exec ["setup", e, "-p", p] = getPassword >>= run . setupOrigin e p 
exec ["setup", e]          = undefined                              -- setup (encrypted dropbox file)                                           
exec ["add", s, k]         = getPassword >>= run . add s k          -- add (sitename) (key)                                                     
exec ["key", s]            = getPassword >>= run . key s            -- key (sitename)                                                           
exec ["list"]              = undefined
exec ["-h"]                = undefined
exec _                     = undefined

run :: App () -> IO ()
run app = runExceptT (runApp app) >>= either putStrLn return

-- command semantics

-- |Takes filepath to location of where to store encrypted file, existing unencrypted passwordfile and password
-- and encrypts file and stores paths to both in lockfile.
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> Password -> App ()
setupOrigin encryptedPath passwordPath p = do
  passwordFile <- exist passwordPath
  encrypted <- liftIO $ encrypt passwordFile p
  liftIO $ BS.writeFile encryptedPath encrypted
  c <- configPath
  liftIO $ writeFile c (intercalate [delim] [encryptedPath, passwordPath])

-- TODO: incomplete (needs parse of format)
key :: Site -> Password -> App ()
key s p = do
  f <- encryptedFile
  f' <- liftIO $ CBC.decrypt (pack p) f
  case parse f' of
    Nothing -> throw "Something went wrong with parsing"
    Just xs -> liftIO $ print (findKey s xs) -- Maybe Text
  -- liftIO $ setClipboard (unpack f')

-- TODO: incomplete (needs addition according to format)
add :: Site -> Key -> Password -> App ()
add site key password = do
  f <- encryptedFile
  f' <- decryptFile password f
  liftIO $ TIO.putStrLn f'

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

-- Helpers

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = find (\sd -> name sd == T.pack s) xs >>= siteKey

encryptedFile :: App BS.ByteString
encryptedFile = configFilePaths >>= liftIO . BS.readFile . head

configFilePaths :: App [FilePath]
configFilePaths = do
  cf <- configurationFile
  s <- liftIO $ readFile cf
  if s /= "" then mapM exist (split s) else throw "Empty config"

decryptFile :: Password -> BS.ByteString -> App T.Text
decryptFile p f = do
  let p' = pack p
  f' <- liftIO $ CBC.decrypt p' f
  return $ decodeUtf8 f'

configurationFile :: App FilePath
configurationFile = configPath >>= exist

configPath :: App FilePath
configPath = flip (</>) lockfile <$> liftIO getHomeDirectory

concatFilePaths :: FilePath -> FilePath -> String
concatFilePaths f p = f ++ (delim:p)

exist :: FilePath -> App FilePath
exist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return path
    else throw $ "No such file: " ++ path

throw :: Error -> App a
throw s = App $ throwE s

-- appends 0:s until 17 characters have been reached
getPassword :: IO Password
getPassword = do
  putStrLn "enter password"
  l <- getLine
  return $ padR (17 - length l) '0' l

split :: String -> [String]
split s =
  case dropWhile (== delim) s of
    "" -> []
    s' -> w : split s''
      where (w, s'') = break (== delim) s'

padR :: Int -> Char -> String -> String
padR n c cs
  | n > 0 = cs ++ replicate n c
  | otherwise = cs
