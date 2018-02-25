module Keychain.Main (keychain) where

import           Keychain.Operations              
import           Keychain.Core              
import           System.Environment         (getArgs)
import           System.Directory           (getHomeDirectory)

keychain :: IO ()
keychain = do
  a <- getArgs
  h <- getHomeDirectory
  run (parse a h)

parse :: [String] -> FilePath -> App ()
parse ["setup", p, "-e", e] h = setupOrigin p (Config e h)
parse ["setup", e, "-p", p] h = setupRemote p (Config e h)
parse ["user", s]           h = config h >>= siteUser s
parse ["key", s]            h = config h >>= siteKey s
parse ["list"]              h = config h >>= list
parse ["-h"]                _ = undefined
parse _                     _ = undefined
