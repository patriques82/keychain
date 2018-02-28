{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Keychain.Core              (SiteDetails(..), Config(..), runWithPassword, siteDetails) 
import           Keychain.Operations
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Exception          (finally)
import           System.FilePath.Posix      ((</>))
import           System.Directory           (getTemporaryDirectory, createDirectory, removeDirectoryRecursive) 
import           System.Hclip               (clearClipboard, getClipboard)

password :: String
password = "12345678901234567" -- password must be over 16 characters

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
  let dir = tmp </> "keychain"
      passwords = dir </> "passwords"
      encrypted = dir </> "encrypted"
      conf = Config encrypted dir
  setup conf passwords
  finally (defaultMain $ testGroup "Keychain tests" (tests conf))
          (removeDirectoryRecursive dir)

setup :: Config -> FilePath -> IO ()
setup conf passwords = do
  createDirectory (home conf)
  writeFile passwords yaml 
  runWithPassword (setupOrigin passwords conf) password
  return ()

tests :: Config -> [TestTree]
tests conf = 
  [ siteDetailsTest conf
  , siteKeyTest conf
  , siteUserTest conf
  , setupRemoteTest conf
  ]

siteKeyTest :: Config -> TestTree
siteKeyTest conf = testCase "siteKey" $ do
  runWithPassword (siteKey "facebook" conf) password
  content <- getClipboard
  clearClipboard
  content @?= "test123"

siteUserTest :: Config -> TestTree
siteUserTest conf = testCase "siteUser" $ do
  runWithPassword (siteUser "gmail" conf) password
  content <- getClipboard
  clearClipboard
  content @?= "prettyBoy"

siteDetailsTest :: Config -> TestTree
siteDetailsTest conf = testCase "siteDetails" $ do
  e <- runWithPassword (siteDetails conf) password
  case e of
    Left _ -> assertBool "Not able to parse" False
    Right xs -> fmap name xs @?= ["facebook", "gmail"]

setupRemoteTest :: Config -> TestTree
setupRemoteTest conf = testCase "setupRemote" $ do
  let remoteDir = home conf </> "remote"
      passwords = remoteDir </> "passwords"
  createDirectory remoteDir
  runWithPassword (setupRemote passwords (conf { home = remoteDir })) password
  rp <- readFile passwords
  p <- readFile $ home conf </> "passwords"
  rp @?= p
