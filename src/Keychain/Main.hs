module Keychain.Main (keychain) where

import           Keychain.Operations              
import           Keychain.Core              
import           System.Environment         (getArgs)
import           System.Directory           (getHomeDirectory)
import           Options.Applicative
import           Data.Monoid                ((<>))

keychain :: IO ()
keychain = do
  c <- execParser opts
  h <- getHomeDirectory
  run (execCommand c h)
  where opts = info (helper <*> input) desc
        desc = fullDesc 
            <> progDesc "encrypt and decrypt passwords" 
            <> header "keychain - a program to keep your passwords out of the open"

data Command = SetupOrigin FilePath FilePath 
             | SetupRemote FilePath FilePath
             | SiteUser String
             | SiteKey String
             | List

execCommand :: Command -> FilePath -> App ()
execCommand (SetupOrigin p e) h = setupOrigin p (Config e h)
execCommand (SetupRemote e p) h = setupRemote p (Config e h)
execCommand (SiteUser s)      h = config h >>= siteUser s
execCommand (SiteKey s)       h = config h >>= siteKey s
execCommand List              h = config h >>= list

input :: Parser Command 
input = setupOriginP 
    <|> setupRemoteP
    <|> siteUserP
    <|> siteKeyP
    <|> listP

setupOriginP :: Parser Command
setupOriginP = SetupOrigin
           <$> strOption setupOpt
           <*> strOption encryptionOpt
  where setupOpt = long "setup"
                <> metavar "PASSWORDS"
                <> help "passwords yaml file location"
        encryptionOpt = long "encrypted"
                     <> short 'e'
                     <> metavar "ENCRYPTED"
                     <> help "target location for encrypted passwords file"

setupRemoteP :: Parser Command
setupRemoteP = SetupRemote 
           <$> strOption setupOpt
           <*> strOption passwordsOpt
  where setupOpt = long "remote"
                <> metavar "ENCRYPTED"
                <> help "encrypted file location"
        passwordsOpt = long "passwords"
                    <> short 'p'
                    <> metavar "PASSWORDS"
                    <> help "target location for unecnrypted passwords file"

siteUserP :: Parser Command
siteUserP = SiteUser <$> strOption userOpt
  where userOpt = long "user" 
               <> short 'u' 
               <> metavar "SITE" 
               <> help "username for site"

siteKeyP :: Parser Command
siteKeyP = SiteKey <$> strOption keyOpt
  where keyOpt = long "key" 
              <> short 'k' 
              <> metavar "SITE" 
              <> help "password for site"

listP :: Parser Command
listP = flag' List opt
  where opt = long "list" 
           <> short 'l' 
           <> help "List all sites available"