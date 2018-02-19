{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Keychain.Core where

import           GHC.Generics               (Generic)
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (pack)
import           Data.List                  (find)
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y 
import           Control.Exception          (bracket_)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import qualified Crypto.Simple.CBC          as CBC
import           System.Directory           (doesFileExist, doesDirectoryExist, getHomeDirectory)
import           System.FilePath.Posix      ((</>), takeDirectory, isValid)
import           System.IO                  (hFlush, hSetEcho, stdout, stdin)

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

newtype App a = App {
  runApp :: ReaderT Password (ExceptT Error IO) a
} deriving (Functor, Applicative, Monad, MonadIO)

type PasswordApp = App () 

run :: PasswordApp -> IO ()
run app = getPassword >>= run' >>= either putStrLn return
  where run' = runExceptT . runReaderT (runApp app)

passwordApp :: ([SiteDetails] -> App ()) -> PasswordApp
passwordApp f = do
  e <- encryptedFilePath
  s <- decrypt e
  case Y.decode s of
    Nothing -> throw "Incorrect password"
    Just xs -> f xs

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

write :: BS.ByteString -> FilePath -> App ()
write b f = do
  assertWritable f
  liftIO $ BS.writeFile f b

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
  if isValid path
    then do 
      let dir = takeDirectory path
      exists <- liftIO $ doesDirectoryExist dir
      if exists 
        then return ()
        else throw $ "Directory " ++ dir ++ " does not exist"
    else throw $ "Path " ++ path ++ " is not valid"

assertExist :: FilePath -> App ()
assertExist path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then return ()
    else throw $ "File " ++ path ++ " does not exist"

throw :: Error -> App a
throw = App . lift . throwE

password :: App Password
password = App $ ask

-- helper functions

getPassword :: IO Password
getPassword = do
  putStr "Password: "
  hFlush stdout
  p <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine 
  putChar '\n'
  return $ padR (17 - length p) '0' p

padR :: Int -> Char -> String -> String
padR n c cs
  | n > 0 = cs ++ replicate n c
  | otherwise = cs

findSite :: Site -> [SiteDetails] -> Maybe SiteDetails
findSite s xs = find ((==) (T.pack s) . name) xs

findUser :: Site -> [SiteDetails] -> Maybe T.Text
findUser s xs = findSite s xs >>= user

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = findSite s xs >>= key