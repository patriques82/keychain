module Keychain.Main (keychain) where

import           Keychain.Operations              
import           Keychain.Core              
import           System.Environment         (getArgs)

keychain :: IO ()
keychain = getArgs >>= runPasswordApp . parse

parse :: [String] -> PasswordApp
parse ["setup", e, "-p", p] = setupOrigin e p
parse ["setup", e]          = setupRemote e
parse ["user", s]           = siteUser s
parse ["key", s]            = siteKey s
parse ["list"]              = list
parse ["sync", p]           = sync p
parse ["-h"]                = undefined
parse _                     = undefined