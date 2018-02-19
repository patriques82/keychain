module Keychain.Operations where 

import           Keychain.Core
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Char8      (pack)
import qualified Data.Text.IO               as TIO
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y 
import           System.Hclip               (setClipboard)

-- | Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> PasswordApp
setupOrigin encryptedPath passwordPath = do
  encrypted <- encrypt passwordPath
  write encrypted encryptedPath 
  c <- configPath
  write (pack encryptedPath) c

-- | Takes filepath to location of existing encrypted file and stores path to encrypted file in lockfile (config).
setupRemote :: EncryptedFilePath -> PasswordApp
setupRemote e = passwordApp $ \_ -> configPath >>= write (pack e)

-- | Adds the key for given site to the clipboard.
siteKey :: Site -> PasswordApp
siteKey s = passwordApp $ \xs -> 
  case findKey s xs of
    Nothing -> liftIO $ putStrLn $ "Site " ++ s ++ " not found"
    Just k -> liftIO $ setClipboard (T.unpack k) 

-- | Adds the username for given site to the clipboard.
siteUser :: Site -> PasswordApp
siteUser s = passwordApp $ \xs -> 
  case findUser s xs of
    Nothing -> liftIO $ putStrLn $ "Site " ++ s ++ " not found"
    Just u -> liftIO $ setClipboard (T.unpack u) 

-- | Lists name of all sites in encrypted file
list :: PasswordApp
list = passwordApp $ liftIO . mapM_ (TIO.putStrLn . name)

-- | Unencrypts encryptedfile and stores contents at given filepath
sync :: PasswordFilePath -> PasswordApp
sync p = passwordApp $ \xs -> do
  assertWritable p
  liftIO $ Y.encodeFile p xs