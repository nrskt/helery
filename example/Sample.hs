{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Text
import           Helery

data MyRouter = AHandlerRoute AHandler
              | BHandlerRoute BHandler
               deriving (Eq, Show)

instance Dispatchable MyRouter where
    dispatch (AHandlerRoute handler) = handle handler
    dispatch (BHandlerRoute handler) = handle handler

instance FromJSON MyRouter where
    parseJSON args@(Object v) =
        case HM.lookup "task" v of
            Just (String "AHandler") ->
                AHandlerRoute <$> (parseJSON args :: Parser AHandler)
            Just (String "BHandler") ->
                BHandlerRoute <$> (parseJSON args :: Parser BHandler)
            Nothing -> fail "Not Found routing"


-- ----------------------------------------------------------------------------
-- Define A Handler
-- ----------------------------------------------------------------------------
type AHandler = BaseHandler AHandlerArgs AHandlerKwargs

instance FromJSON AHandler where
    parseJSON (Object v) =
        case HM.lookup "task" v of
            Just (String "AHandler") ->
                BaseHandler <$> pure "AHandler"
                            <*> v .: "args"
                            <*> v .: "kwargs"

instance Handler AHandler where
    handle = print

-- | define Args type.
data AHandlerArgs = AHandlerArgs Int Int deriving (Eq, Show)

instance FromJSON AHandlerArgs where
    parseJSON jsn = do
        (a, b) <- parseJSON jsn
        return $ AHandlerArgs a b

-- | define Kwargs type.
data AHandlerKwargs = AHandlerKwargs
    { aName :: Text
    , aAge  :: Int }
    deriving (Eq, Show)

instance FromJSON AHandlerKwargs where
    parseJSON (Object v) =
        AHandlerKwargs <$> v .: "name"
                       <*> v .: "age"


-- ----------------------------------------------------------------------------
-- | Define B Handler
-- ----------------------------------------------------------------------------
type BHandler = BaseHandler BHandlerArgs BHandlerKwargs

instance FromJSON BHandler where
    parseJSON (Object v) =
        case HM.lookup "task" v of
            Just (String "BHandler") ->
                BaseHandler <$> pure "BHandler"
                            <*> v .: "args"
                            <*> v .: "kwargs"

instance Handler BHandler where
    handle = print

-- | define Args type.
data BHandlerArgs = BHandlerArgs Int Int deriving (Eq, Show)

instance FromJSON BHandlerArgs where
    parseJSON jsn = do
        (a, b) <- parseJSON jsn
        return $ BHandlerArgs a b

-- | define Kwargs type.
data BHandlerKwargs = BHandlerKwargs
    { bName :: Text
    , bAge  :: Int }
    deriving (Eq, Show)

instance FromJSON BHandlerKwargs where
    parseJSON (Object v) =
        BHandlerKwargs <$> v .: "name"
                       <*> v .: "age"

main :: IO ()
main = do
    options <- parseCommandLineOption
    print options  -- Debug for option value
    runApp options (routing :: RoutingType IO MyRouter)
