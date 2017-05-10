{-# LANGUAGE OverloadedStrings #-}
module Helery.MessageQueue
    ( ExtractedQMessage(qArgs, qTaskName)
    , extractQMessage
    , parseArguments
    )where

import           Control.Lens          ((^?))
import           Data.Aeson            (FromJSON, Value, parseJSON)
import           Data.Aeson.Lens       (key)
import           Data.Aeson.Types      (Parser, Result (Error, Success), parse)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromJust)
import           Data.Text             (Text)


-- | Queuing Message data type
data ExtractedQMessage = ExtractedQMessage
    { qTaskName :: Text
    , qArgs     :: Value
    , qKwargs   :: Value
    } deriving (Eq, Show)


-- | Constructor for ExtractedQMessage
-- Create data from ByteString
extractQMessage :: B.ByteString -> ExtractedQMessage
extractQMessage bstr = ExtractedQMessage task args kwargs
    where
        task = case result value of
            Success t -> t
            where
                value = fromJust $ bstr ^? key "task"
                result = parse (parseJSON :: Value -> Parser Text)
        args = fromJust $ bstr ^? key "args"
        kwargs = fromJust $ bstr ^? key "kwargs"


-- | Parse arguments for task handler
parseArguments :: (FromJSON a, FromJSON b)
               => ExtractedQMessage
               -> (a, b)
parseArguments msg = (parseArgs msg, parseKwargs msg)

-- | private function for parseArguments
parseArgs :: FromJSON a
          => ExtractedQMessage
          -> a
parseArgs msg =
    case p msg of
        Success args -> args
    where
        p = parse $ parseJSON . qArgs

-- | private function for parseArguments
parseKwargs :: FromJSON a
            => ExtractedQMessage
            -> a
parseKwargs msg =
    case p msg of
        Success args -> args
    where
        p = parse $ parseJSON . qKwargs
