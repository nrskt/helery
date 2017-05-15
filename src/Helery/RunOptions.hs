{-# LANGUAGE OverloadedStrings #-}
module Helery.RunOptions
    ( Options(..)
    , parseCommandLineOption
    , parseRedisURI
    ) where

import qualified Data.ByteString.Char8     as B
import           Data.Foldable             (msum)
import           Data.Maybe                (fromJust)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Database.Redis            as Redis
import           Network.URI               (URI (..), URIAuth (..), parseURI)
import           Options.Applicative
import           Options.Applicative.Types

-- ----------------------------------------------------------------------------
-- Export Type and Function
-- ----------------------------------------------------------------------------

-- | Option parameter Type
-- This type represent option parameters when running application.
data Options = Options
    { queueName    :: B.ByteString
    , brokerConfig :: String
    , config       :: Maybe FilePath }
    deriving (Show)

-- | parse Option from command line
--
-- use as below
--
-- main :: IO ()
-- main = do
--     options <- parseCommandLineOption
--
parseCommandLineOption :: IO Options
parseCommandLineOption = execParser optionsParserInfo

-- | parse redis URI
--
-- uri = "redis://:password@hostname:port/db_num" -> Redis.ConnectInfo
-- connectMaxConnections and connectMaxIdleTime values are default.
parseRedisURI :: String -> Redis.ConnectInfo
parseRedisURI s = Redis.defaultConnectInfo
    { Redis.connectHost = host s
    , Redis.connectPort = Redis.PortNumber (read $ port s)
    , Redis.connectDatabase = read $ database s
    , Redis.connectMaxConnections = 50
    , Redis.connectMaxIdleTime = 30 }
    where
        host s = fromJust $ uriRegName <$> authority s
        port s = removeColon . fromJust $ uriPort <$> authority s
        auth s = removeAtColon . fromJust $ uriUserInfo <$> authority s
        database s = removeChar '/' . fromJust $ path s

        authority :: String -> Maybe URIAuth
        authority s = msum $ uriAuthority <$> parseURI s

        path :: String -> Maybe String
        path s = uriPath <$> parseURI s


-- ----------------------------------------------------------------------------
-- Option parser
-- ----------------------------------------------------------------------------

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info optionsP
    $ fullDesc
   <> progDesc "run distributed task queue worker"


optionsP :: Parser Options
optionsP = (<*>) helper $
    Options <$> queueNameP <*> brokerConfigP <*> configP

queueNameP :: Parser B.ByteString
queueNameP = bytestringOption
    $ short 'q'
   <> long "queue"
   <> help "queue name"
   <> metavar "NAME"
   <> value "hsworker"
   <> showDefaultWith B.unpack


brokerConfigP :: Parser String
brokerConfigP = strOption
    $ short 'b'
   <> long "broker"
   <> help "brokerURL the format is redis://:password@hostname:port/db_number"
   <> metavar "URL"
   <> value "redis://localhost:6379/0"
   <> showDefaultWith id

configP :: Parser (Maybe FilePath)
configP = optional . strOption
    $ short 'c'
   <> long "config"
   <> help "configuration file path"
   <> metavar "FILE"

bytestring :: ReadM B.ByteString
bytestring = B.pack <$> readerAsk

bytestringOption :: Mod OptionFields B.ByteString -> Parser B.ByteString
bytestringOption = option bytestring

redisUri :: ReadM Redis.ConnectInfo
redisUri = parseRedisURI <$> readerAsk

redisUriOption :: Mod OptionFields Redis.ConnectInfo
               -> Parser Redis.ConnectInfo
redisUriOption = option redisUri

-- remove specific char from string
removeChar :: Char -> String -> String
removeChar c [] = []
removeChar c (x:xs) =
    if c == x
        then removeChar c xs
        else x : removeChar c xs

-- | remove @ char
removeAt = removeChar '@'

-- | remove : char
removeColon = removeChar ':'

-- | remove : and @ char
removeAtColon = removeAt . removeColon
