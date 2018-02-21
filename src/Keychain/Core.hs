{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Keychain.Core where

import qualified Prelude                    as P
import           Prelude                    hiding (writeFile, readFile)
import           GHC.Generics               (Generic)
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (pack)
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y 
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import qualified Crypto.Simple.CBC          as CBC
import qualified System.Directory           as SD (doesFileExist, doesDirectoryExist, getHomeDirectory)
import           System.FilePath.Posix      ((</>), takeDirectory, isValid)

type Error = String
type EncryptedFilePath = FilePath
type PasswordFilePath = FilePath
type Password = String
type Site = String

data SiteDetails = SiteDetails {
  name :: T.Text,
  key :: Maybe T.Text,
  user :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance Y.ToJSON SiteDetails

instance Y.FromJSON SiteDetails

class Monad m => IOProxy m where 
  encryptFile :: FilePath -> Password -> App m BS.ByteString
  decryptFile :: FilePath -> Password -> App m BS.ByteString
  writeFile :: FilePath -> BS.ByteString -> App m ()
  readFile :: FilePath -> App m String
  getHomeDirectory :: App m String
  doesFileExist :: FilePath -> App m Bool
  doesDirectoryExist :: FilePath -> App m Bool

instance IOProxy IO where 
  encryptFile f p = App $ liftIO $ BS.readFile f >>= CBC.encrypt (pack p)
  decryptFile f p = App $ liftIO $ BS.readFile f >>= CBC.decrypt (pack p)
  writeFile f b = App $ liftIO $ BS.writeFile f b
  readFile f = App $ liftIO $ P.readFile f
  getHomeDirectory = App $ liftIO SD.getHomeDirectory
  doesDirectoryExist f = App $ liftIO $ SD.doesDirectoryExist f
  doesFileExist f = App $ liftIO $ SD.doesFileExist f

newtype App m a = App {
  runApp :: (IOProxy m) => ReaderT Password (ExceptT Error m) a
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
  liftIO io = App $ ReaderT $ \_ -> ExceptT (fmap Right (liftIO io))

type PasswordApp = App IO () 

-- run

runPasswordApp :: PasswordApp -> Password -> IO ()
runPasswordApp app p = runExceptT (runReaderT (runApp app) p) >>= either putStrLn return

-- combinators

crypt :: IOProxy m => (FilePath -> Password -> App m BS.ByteString) -> FilePath -> App m BS.ByteString
crypt f fp = assertExist fp >> password >>= f fp

encrypt :: IOProxy m => FilePath -> App m BS.ByteString
encrypt = crypt encryptFile

decrypt :: IOProxy m => FilePath -> App m BS.ByteString
decrypt = crypt decryptFile

write :: IOProxy m => BS.ByteString -> FilePath -> App m ()
write b f = assertWritable f >> writeFile f b

encryptedFilePath :: IOProxy m => App m FilePath
encryptedFilePath = do
  c <- configPath
  e <- readFile c
  case lines e of
    (e':[]) -> return e'
    _ -> throw "Config does not contain file path"

configPath :: IOProxy m => App m FilePath
configPath = flip (</>) ".lockfile" <$> getHomeDirectory

assertWritable :: IOProxy m => FilePath -> App m ()
assertWritable path = do
  if isValid path
    then do 
      let dir = takeDirectory path
      exists <- doesDirectoryExist dir
      if exists 
        then return ()
        else throw $ "Directory " ++ dir ++ " does not exist"
    else throw $ "Path " ++ path ++ " is not valid"

assertExist :: IOProxy m => FilePath -> App m ()
assertExist path = do
  exists <- doesFileExist path
  if exists
    then return ()
    else throw $ "File " ++ path ++ " does not exist"

throw :: IOProxy m => Error -> App m a
throw = App . lift . throwE

password :: IOProxy m => App m Password
password = App $ ask