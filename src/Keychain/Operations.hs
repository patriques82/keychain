module Keychain.Operations where 

import           Keychain.Core
import           Prelude                    hiding (writeFile, readFile)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Char8      (pack)
import qualified Data.Text.IO               as TIO
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y 
import           System.Hclip               (setClipboard)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)

-- |Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: EncryptedFilePath -> PasswordFilePath -> PasswordApp
setupOrigin encryptedPath passwordPath = do
  encrypted <- encryptFile passwordPath
  writeFile encryptedPath encrypted
  c <- configPath
  writeFile c (pack encryptedPath)

-- |Takes filepath to location of existing encrypted file and stores path to encrypted file in lockfile (config).
setupRemote :: EncryptedFilePath -> PasswordApp
setupRemote e = passwordApp $ const $ configPath >>= flip writeFile (pack e)

-- |Adds the key for given site to the clipboard.
siteKey :: Site -> PasswordApp
siteKey s = passwordApp $ \xs -> 
  case findKey s xs of
    Nothing -> liftIO $ putStrLn $ "Site " ++ s ++ " not found"
    Just k -> liftIO $ setClipboard (T.unpack k) 

-- |Adds the username for given site to the clipboard.
siteUser :: Site -> PasswordApp
siteUser s = passwordApp $ \xs -> 
  case findUser s xs of
    Nothing -> liftIO $ putStrLn $ "Site " ++ s ++ " not found"
    Just u -> liftIO $ setClipboard (T.unpack u) 

-- |Lists name of all sites in encrypted file
list :: PasswordApp
list = passwordApp $ liftIO . mapM_ (TIO.putStrLn . name)

-- |Unencrypts encryptedfile and stores contents at given filepath
sync :: PasswordFilePath -> PasswordApp
sync fp = passwordApp $ liftIO . Y.encodeFile fp

-- helper functions

passwordApp :: ([SiteDetails] -> App IO ()) -> PasswordApp
passwordApp f = do
  xs <- fmap Y.decode $ decryptFile =<< encryptedFilePath
  fromMaybe (throwApp WrongPassword) (f <$> xs)

findUser :: Site -> [SiteDetails] -> Maybe T.Text
findUser s xs = findSite s xs >>= user

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = findSite s xs >>= key

findSite :: Site -> [SiteDetails] -> Maybe SiteDetails
findSite s xs = find ((==) (T.pack s) . name) xs