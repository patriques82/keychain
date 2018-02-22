{-# LANGUAGE OverloadedStrings #-}

module Keychain.Operations where 

import           Keychain.Core
import           Prelude                    hiding (writeFile, readFile)
import           Data.ByteString.Char8      (pack)
import qualified Data.Text                  as T
import           Data.List                  (find)

-- |Takes filepath to location of where to store encrypted file, existing unencrypted password file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: IOProxy m => EncryptedFilePath -> PasswordFilePath -> App m ()
setupOrigin encryptedPath passwordPath = do
  encrypted <- encryptFile passwordPath
  writeFile encryptedPath encrypted
  c <- configPath
  writeFile c (pack encryptedPath)

-- |Takes filepath to location of existing encrypted file and stores path to encrypted file in lockfile (config).
setupRemote :: IOProxy m => EncryptedFilePath -> App m ()
setupRemote e = passwordApp $ const $ configPath >>= flip writeFile (pack e)

-- |Adds the key for given site to the clipboard.
siteKey :: IOProxy m => Site -> App m ()
siteKey s = passwordApp $ \xs -> 
  case findKey s xs of
    Nothing -> printText $ T.unwords ["Site ", T.pack s, " not found"]
    Just k -> copy (T.unpack k) 

-- |Adds the username for given site to the clipboard.
siteUser :: IOProxy m => Site -> App m ()
siteUser s = passwordApp $ \xs -> 
  case findUser s xs of
    Nothing -> printText $ T.unwords ["Site ", T.pack s, " not found"]
    Just u -> copy (T.unpack u) 

-- |Lists name of all sites in encrypted file
list :: IOProxy m => App m ()
list = passwordApp $ mapM_ (printText . name)

-- |Unencrypts encryptedfile and stores contents at given filepath
sync :: IOProxy m => PasswordFilePath -> App m ()
sync fp = passwordApp $ encodeFile fp

-- helper functions

findUser :: Site -> [SiteDetails] -> Maybe T.Text
findUser s xs = findSite (T.pack s) xs >>= user

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = findSite (T.pack s) xs >>= key

findSite :: T.Text -> [SiteDetails] -> Maybe SiteDetails
findSite s xs = find ((==) s . name) xs