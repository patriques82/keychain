{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Crypto.Simple.CBC          as CBC (decrypt, encrypt)
import qualified Data.ByteString            as BS (ByteString, readFile, writeFile)
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.IO               as TIO (putStrLn)
import           Debug.Trace                (trace) -- TODO remove
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment         (getArgs)
import           System.FilePath.Posix      ((</>))
import           System.Hclip               (setClipboard)

-- Types

type Error = String

newtype App a = App {
  runApp :: ExceptT Error IO a  -- IO (Either Error a)
} deriving (Functor, Applicative, Monad, MonadIO)

data Command = SetupFile String String -- | encrypted file, raw passwords file (TODO optional?)
             | Setup String            -- | encrypted file
             | Add String String
             | Key String
             | Encrypt
             | List
             | Usage
             | Help
  deriving (Eq, Show)

-- TODO: replace with (FilePath -> Text -> IO ())
data Config = Config {
  filePath :: FilePath , -- | ~/.lockfile
  contents :: String
}

-- Globals

delim = ':'

-- Main

main :: IO ()
main = getArgs >>= exec . parse

parse :: [String] -> Command
parse ["-c", encrypted, "-p", passwords] = SetupFile encrypted passwords
parse ["-c", encrypted]                  = Setup encrypted
parse ["-a", site, key]                  = Add site key
parse ["-k", site]                       = Key site
parse ["encrypt"]                        = Encrypt
parse ["-l"]                             = List
parse ["-h"]                             = Help
parse _                                  = Usage

exec :: Command -> IO ()
exec (SetupFile f p) = setup $ Config <$> configurationFile <*> configContents f p
exec (Setup f) = setup $ Config <$> configurationFile <*> exist f
exec (Add s k) = getPassword >>= add s k
exec (Key s) = getPassword >>= key s
exec Encrypt = getPassword >>= encrypt
exec List = undefined -- getEncryptionFile >>= list

setup :: App Config -> IO ()
setup conf = run conf $ \(Config f c) -> writeFile f c

add :: String -> String -> String -> IO ()
add site key password = run (insert site key password) return

key :: String -> String -> IO ()
key site password = run (extract site password) setClipboard

encrypt :: String -> IO ()
encrypt password = run (encrypt' password) return

run :: App a -> (a -> IO ()) -> IO ()
run app f = (runExceptT . runApp $ app) >>= either putStrLn f

-- Primitives

extract :: String -> String -> App String
extract site password = do
  f <- encryptedFile
  f' <- liftIO $ CBC.decrypt (pack password) f
  return (unpack f')

insert :: String -> String -> String -> App ()
insert site key password = do
  f <- encryptedFile
  f' <- decryptFile password f
  liftIO $ TIO.putStrLn f'

encrypt' :: String -> App ()
encrypt' password = do
  ps' <- configFilePaths
  let ps = trace "hhh" ps'
  case ps of
    (e:p:_) -> do
      f <- liftIO $ BS.readFile p
      f' <- liftIO $ CBC.encrypt (pack password) f
      liftIO $ BS.writeFile e f'
    otherwise -> throw "Incomplete config for encryption: need a password file"

checkFormat :: BS.ByteString -> App BS.ByteString
checkFormat cs = undefined

encryptedFile :: App BS.ByteString
encryptedFile = configFilePaths >>= liftIO . BS.readFile . head

configFilePaths :: App [FilePath]
configFilePaths = do
  cf <- configurationFile
  s <- liftIO $ readFile cf
  if (s /= "")
    then mapM exist (split s)
    else throw "Empty config"

decryptFile :: String -> BS.ByteString -> App Text
decryptFile p f = do
  let p' = pack p
  f' <- liftIO $ CBC.decrypt p' f
  return $ decodeUtf8 f'

configurationFile :: App String
configurationFile = flip (</>) ".lockfile" <$> liftIO getHomeDirectory

configContents :: FilePath -> FilePath -> App String
configContents f p = do
  f' <- exist f
  p' <- exist p
  return $ f' ++ (delim:p')

exist :: FilePath -> App FilePath
exist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return path
    else throw $ "No such file: " ++ path

throw :: Error -> App a
throw s = App $ throwE s

-- Helpers

-- appends 0:s until 17 characters have been reached
getPassword :: IO String
getPassword = do
  putStrLn "enter password"
  l <- getLine
  let p = padR (17 - length l) '0' l
  return p

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

