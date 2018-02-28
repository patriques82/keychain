module Main where

import           Keychain.Core              (runWithPassword, Config(..))
import           Keychain.Operations
import           Control.Exception
import           Control.Exception.Base
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.FilePath.Posix      ((</>))
import           System.Directory           (getHomeDirectory, getTemporaryDirectory, createDirectory, removeDirectoryRecursive, removePathForcibly) 
import           System.Hclip               (clearClipboard, getClipboard)

password :: String
password = "12345678901234567"

yaml :: String
yaml = "- user: user89\n\
       \  key: test123\n\
       \  name: facebook\n\
       \- user: prettyBoy\n\
       \  key: testingthapass\n\
       \  name: gmail"

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "keychain2"
      passwords = dir </> "passwords"
      encrypted = dir </> "encrypted"
  setup dir passwords encrypted
  finally (defaultMain $ testGroup "Keychain tests" [siteKeyTest dir encrypted])
          (removeDirectoryRecursive dir)

setup :: FilePath -> FilePath -> FilePath -> IO ()
setup dir passwords encrypted = do
  createDirectory dir
  writeFile passwords yaml 
  runWithPassword (setupOrigin passwords (Config encrypted dir)) password
  return ()

siteKeyTest :: FilePath -> FilePath -> TestTree
siteKeyTest dir encryptedFile = testCase "Testing siteKey" $ do
  runWithPassword (siteKey "facebook" (Config encryptedFile dir)) password
  content <- getClipboard
  clearClipboard
  content @?= "test123"

-- full flow
