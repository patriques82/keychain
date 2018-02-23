{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Keychain.Core where

import qualified Prelude                    as P
import           Prelude                    hiding (writeFile, readFile)
import           GHC.Generics               (Generic)
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (pack)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Yaml                  as Y 
import           Control.Exception          (Exception, SomeException, try, toException, fromException)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import qualified Crypto.Simple.CBC          as CBC
import qualified System.Directory           as SD (getHomeDirectory)
import           System.FilePath.Posix      ((</>))
import           System.Hclip               (setClipboard)
import           System.IO                  (hFlush, hSetEcho, stdout, stdin)
import           Control.Exception          (bracket_)
import           Data.Maybe                 (fromMaybe)

type EncryptedFilePath = FilePath
type PasswordFilePath = FilePath
type Password = String
type Site = String
type PasswordApp = App IO ()

data SiteDetails = SiteDetails {
  name :: T.Text,
  key :: Maybe T.Text,
  user :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance Y.ToJSON SiteDetails

instance Y.FromJSON SiteDetails

-- primitives 

class Monad m => IOProxy m where 
  encryptFile :: FilePath -> App m BS.ByteString
  decryptFile :: FilePath -> App m BS.ByteString
  writeFile :: FilePath -> BS.ByteString -> App m ()
  readFile :: FilePath -> App m String
  getHomeDirectory :: App m String
  encodeFile :: Y.ToJSON a => FilePath -> [a] -> App m ()
  printText :: T.Text -> App m ()
  copy :: String -> App m ()

instance IOProxy IO where 
  encryptFile f = label EncryptionException $ try . (>>=) (BS.readFile f) . CBC.encrypt . pack
  decryptFile f = label DecryptionException $ try . (>>=) (BS.readFile f) . CBC.decrypt . pack
  writeFile f = label WriteFileException . const . try . BS.writeFile f
  readFile = label ReadFileException . const . try . P.readFile
  getHomeDirectory = label NoSuchDirectoryException $ const (try SD.getHomeDirectory)
  encodeFile f = label EncodingException . const . try . Y.encodeFile f
  printText = label PrintException . const . try . TIO.putStrLn
  copy = label ClipboardException . const . try . setClipboard

label :: AppException -> (Password -> IO (Either SomeException a)) -> App IO a 
label e f = App $ ReaderT $ ExceptT . fmap (first (const e)) . f 

data AppException
  = EncryptionException
  | DecryptionException
  | WriteFileException
  | ReadFileException
  | NoSuchDirectoryException
  | IncorrectConfigFormat
  | WrongPassword
  | ClipboardException
  | EncodingException
  | PrintException

instance Show AppException where
  show EncryptionException = "Error encrypting file"
  show DecryptionException = "Error decrypting file"
  show WriteFileException = "Error writing to file"
  show ReadFileException = "Error reading from file"
  show NoSuchDirectoryException = "Error reding directory"
  show IncorrectConfigFormat = "Incorrect format of lockfile"
  show WrongPassword = "Wrong password, try again"
  show ClipboardException = "Error copying to clipboard"
  show PrintException = "Error printing to terminal"

instance Exception AppException

newtype App m a = App {
  runApp :: (IOProxy m) => ReaderT Password (ExceptT AppException m) a
} deriving (Functor)

instance Applicative m => Applicative (App m) where
  pure x = App (ReaderT $ \_ -> (ExceptT (pure (Right x))))
  (App f) <*> (App x) = App $ ReaderT $ \p -> 
    let f' = runExceptT ((runReaderT f) p)
        x' = runExceptT ((runReaderT x) p)
    in ExceptT $ (fmap (<*>)) f' <*> x' 

instance Monad m => Monad (App m) where
  return = pure
  (App x) >>= f = App $ ReaderT $ \p ->
    let mx = runExceptT ((runReaderT x) p)
    in ExceptT $ mx >>= (\x' -> 
      case x' of
        Right y -> runExceptT (runReaderT (runApp (f y)) p)
        Left e -> return (Left e))

instance MonadIO m => MonadIO (App m) where 
  liftIO = App . liftIO

-- run { App IO () }

nrOfTries :: Int
nrOfTries = 3

run :: PasswordApp -> IO ()
run app = go 0 app 

go :: Int -> PasswordApp -> IO ()
go n app = do 
  p <- getPassword
  e <- runExceptT $ runReaderT (runApp app) p
  either handleException return e
  where handleException :: AppException -> IO ()
        handleException WrongPassword = 
          if n > (nrOfTries - 1) 
            then return () 
            else putStrLn (show WrongPassword) >> go (n+1) app
        handleException e = putStrLn (show e)

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  p <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine 
  putChar '\n'
  return $ padR (17 - length p) '0' p
  where padR n c cs
          | n > 0 = cs ++ replicate n c
          | otherwise = cs

-- combinators

passwordApp :: IOProxy m => ([SiteDetails] -> App m ()) -> App m ()
passwordApp f = do
  xs <- fmap Y.decode $ decryptFile =<< encryptedFilePath
  fromMaybe (throwApp WrongPassword) $ fmap f xs

encryptedFilePath :: IOProxy m => App m FilePath
encryptedFilePath = do
  c <- configPath
  e <- readFile c
  case lines e of
    [fp] -> return fp
    _ -> throwApp IncorrectConfigFormat

configPath :: IOProxy m => App m FilePath
configPath = flip (</>) ".lockfile" <$> getHomeDirectory

throwApp :: IOProxy m => AppException -> App m a
throwApp e = App $ lift $ ExceptT (pure (Left e))

password :: IOProxy m => App m Password
password = App $ ask
