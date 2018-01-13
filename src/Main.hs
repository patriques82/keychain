{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Crypto.Simple.CBC          as CBC (decrypt, encrypt)
import qualified Data.ByteString            as BS (ByteString, readFile,
                                                   writeFile)
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.IO               as TIO (putStrLn)
import           Debug.Trace                (trace)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment         (getArgs)
import           System.FilePath.Posix      ((</>))
import           System.Hclip               (setClipboard)

-- Types

type Error = String
type EncryptedFilePath = FilePath
type PasswordFilePath = FilePath
type Password = String
type Site = String
type Key = String

newtype App a = App {
  runApp :: ExceptT Error IO a  -- IO (Either Error a)
} deriving (Functor, Applicative, Monad, MonadIO)

delim = ':'
lockfile = ".lockfile"

-- Main

main :: IO ()
main = getArgs >>= exec

exec :: [String] -> IO ()
exec ["setup", e, "-p", p] = run $ setupOrigin e p
exec ["setup", e]          = undefined
exec ["add", s, k]         = getPassword >>= run . add s k
exec ["key", s]            = getPassword >>= run . key s
exec ["encrypt"]           = getPassword >>= run . encrypt
exec ["list"]              = undefined
exec ["-h"]                = undefined
exec _                     = undefined

run :: App () -> IO ()
run app = (runExceptT . runApp $ app) >>= either putStrLn return

-- command semantics

setupOrigin :: EncryptedFilePath -> PasswordFilePath -> App ()
setupOrigin e p = do
  f <- configurationFile
  c <- concatFilePaths e p
  liftIO $ writeFile f c

-- TODO: incomplete
key :: Site -> Password -> App ()
key s p = do
  f <- encryptedFile
  f' <- liftIO $ CBC.decrypt (pack p) f
  liftIO $ setClipboard (unpack f')

-- TODO: incomplete
add :: Site -> Key -> Password -> App ()
add s k p = do
  f <- encryptedFile
  f' <- decryptFile p f
  liftIO $ TIO.putStrLn f'

encrypt :: Password -> App ()
encrypt password = do
  ps <- configFilePaths
  case ps of
    (e:p:_) -> liftIO $ do
      f <- BS.readFile p
      f' <- CBC.encrypt (pack password) f
      BS.writeFile e f'
    otherwise -> throw "Incomplete config for encryption: need a password file"

-- Helpers

checkFormat :: BS.ByteString -> App BS.ByteString
checkFormat bs = undefined

encryptedFile :: App BS.ByteString
encryptedFile = configFilePaths >>= liftIO . BS.readFile . head

configFilePaths :: App [FilePath]
configFilePaths = do
  cf <- configurationFile
  s <- liftIO $ readFile cf
  if (s /= "")
    then mapM exist (split s)
    else throw "Empty config"

decryptFile :: Password -> BS.ByteString -> App Text
decryptFile p f = do
  let p' = pack p
  f' <- liftIO $ CBC.decrypt p' f
  return $ decodeUtf8 f'

configurationFile :: App FilePath
configurationFile = do
  cf <- flip (</>) lockfile <$> liftIO getHomeDirectory
  exist cf

concatFilePaths :: FilePath -> FilePath -> App String
concatFilePaths f p = do
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

-- appends 0:s until 17 characters have been reached
getPassword :: IO Password
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
