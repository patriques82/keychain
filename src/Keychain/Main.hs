module Keychain.Main (keychain) where

import           Keychain.Operations              
import           Keychain.Core              
import           Control.Exception          (bracket_)
import           System.Environment         (getArgs)
import           System.IO                  (hFlush, hSetEcho, stdout, stdin)

keychain :: IO ()
keychain = getArgs >>= run . parse

parse :: [String] -> PasswordApp
parse ["setup", e, "-p", p] = setupOrigin e p
parse ["setup", e]          = setupRemote e
parse ["user", s]           = siteUser s
parse ["key", s]            = siteKey s
parse ["list"]              = list
parse ["sync", p]           = sync p
parse ["-h"]                = undefined
parse _                     = undefined

run :: PasswordApp -> IO ()
run app = getPassword >>= runPasswordApp app

-- helper functions

getPassword :: IO String
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
