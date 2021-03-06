{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Keychain.Operations (
  setupOrigin, 
  setupRemote,
  siteKey, 
  siteUser, 
  list
) where 

import           Keychain.Core
import           Prelude                    hiding (writeFile, readFile)
import qualified Data.ByteString.Char8      as BC (pack, putStrLn)
import qualified Data.Text                  as T
import           Data.List                  (find)
import           System.FilePath.Posix      ((</>))

-- |Takes filepath to existing unencrypted password file, location of where to store encrypted file
-- and encrypts password file and stores path to encrypted file in lockfile (config).
setupOrigin :: FilePath -> Config -> App ()
setupOrigin passwords Config{..} = do
  let target = home </> encryptedFile
  e <- encryptFile passwords
  writeFile target e
  writeFile (configFile home) (BC.pack target)

-- |Takes filepath to existing encrypted file, location of where to store unencrypted password file
-- and unencrypts encrypted file, stores passwords and path to encrypted file in lockfile (config).
setupRemote :: FilePath -> Config -> App ()
setupRemote passwords Config{..} = do
  let target = home </> encryptedFile
  p <- decryptFile target
  writeFile passwords p
  writeFile (configFile home) (BC.pack target)

-- |Adds the key for given site to the clipboard.
siteKey :: Site -> Config -> App ()
siteKey s c = siteDetails c >>= \xs -> do
  case findKey s xs of
    Nothing -> printText $ T.unwords ["Key for site ", T.pack s, " not found"]
    Just k -> copy (T.unpack k) 

-- |Adds the username for given site to the clipboard.
siteUser :: Site -> Config -> App ()
siteUser s c = siteDetails c >>= \xs -> 
  case findUser s xs of
    Nothing -> printText $ T.unwords ["User for site ", T.pack s, " not found"]
    Just u -> copy (T.unpack u) 

-- |Lists name of all sites in encrypted file
list :: Config -> App ()
list c = siteDetails c >>= mapM_ (printText . name)

-- helper functions

findUser :: Site -> [SiteDetails] -> Maybe T.Text
findUser s xs = findSite (T.pack s) xs >>= user

findKey :: Site -> [SiteDetails] -> Maybe T.Text
findKey s xs = findSite (T.pack s) xs >>= key

findSite :: T.Text -> [SiteDetails] -> Maybe SiteDetails
findSite s xs = find ((==) s . name) xs
