{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           System.Hclip               (setClipboard)
import           System.IO                  (hFlush, hSetEcho, stdout, stdin)
import           Control.Exception          (bracket_)
import           Data.Maybe                 (fromMaybe)
import           System.FilePath.Posix      ((</>))

type Directory = FilePath
type Password = String
type Site = String

data Config = Config {
  encryptedFile :: FilePath ,
  home :: Directory         
} deriving Show

data SiteDetails = SiteDetails {
  name :: T.Text ,         
  key :: Maybe T.Text ,    
  user :: Maybe T.Text     
} deriving (Eq, Show, Generic)

instance Y.ToJSON SiteDetails

instance Y.FromJSON SiteDetails

data AppException
  = IncorrectConfigFormat
  | WrongPassword
  deriving Show

instance Exception AppException

newtype App a = App {
  runApp :: ReaderT Password (ExceptT SomeException IO) a
} deriving (Functor, Applicative, Monad, MonadIO)

-- primitives 

encryptFile :: FilePath -> App BS.ByteString
encryptFile f = App $ ReaderT $ ExceptT . try . (>>=) (BS.readFile f) . CBC.encrypt . pack

decryptFile :: FilePath -> App BS.ByteString
decryptFile f = App $ ReaderT $ ExceptT . try . (>>=) (BS.readFile f) . CBC.decrypt . pack

writeFile :: FilePath -> BS.ByteString -> App ()
writeFile f b = App $ ReaderT $ ExceptT . const (try $ BS.writeFile f b)

readFile :: FilePath -> App String
readFile f = App $ ReaderT $ ExceptT . const (try $ P.readFile f)

encodeFile :: Y.ToJSON a => FilePath -> [a] -> App ()
encodeFile f xs = App $ ReaderT $ ExceptT . const (try $ Y.encodeFile f xs)

printText :: T.Text -> App ()
printText t = App $ ReaderT $ ExceptT . const (try $ TIO.putStrLn t)

copy :: String -> App ()
copy s = App $ ReaderT $ ExceptT . const (try $ setClipboard s)

siteDetails :: Config -> App [SiteDetails]
siteDetails Config{..} = do
  b <- decryptFile (home </> encryptedFile)
  case Y.decode b of
    Just xs -> return xs
    Nothing -> throwApp WrongPassword

configFile :: Directory -> FilePath
configFile home = home </> ".lockfile"

config :: Directory -> App Config
config home = do
  e <- readFile (configFile home)
  case lines e of
    [encryptedFile] -> return Config{..} 
    _ -> throwApp IncorrectConfigFormat

throwApp :: AppException -> App a
throwApp e = App $ lift $ ExceptT (pure (Left $ toException e))

password :: App Password
password = App $ ask

-- run

nrOfTries = 3

run :: App () -> IO ()
run app = go 1 app 

go :: Int -> App () -> IO ()
go n app = do 
  p <- getPassword
  e <- runWithPassword app p
  either handleException return e
  where handleException :: SomeException -> IO ()
        handleException ex = 
          case fromException ex of
            Just WrongPassword -> if n > nrOfTries
                                    then return () 
                                    else putStrLn "wrong password, try again" >> go (n+1) app
            Just IncorrectConfigFormat -> putStrLn "wrong config"
            Nothing -> putStrLn (show ex)

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

runWithPassword :: App a -> Password -> IO (Either SomeException a)
runWithPassword app p = runExceptT $ runReaderT (runApp app) p