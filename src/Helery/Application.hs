{-# LANGUAGE OverloadedStrings #-}
module Helery.Application
    ( runApp
    , routing
    , RoutingType
    ) where


import           Control.Concurrent     (forkIO)
import           Control.Lens           ((^?))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, decode)
import           Data.Aeson.Lens        (key, _String)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as B
import           Data.ByteString.Lazy   (fromStrict)
import           Data.Conduit           (Conduit, ResumableSource, Sink, Source,
                                         await, yield, ($$), ($$+), ($$++),
                                         (=$), (=$=))
import qualified Data.Conduit.List      as CL
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, pack)
import qualified Data.Text.Encoding     as T
import qualified Database.Redis         as Redis

import           Helery.Task            (Dispatchable, dispatch)

-- | Redis key type
type RedisKey = B.ByteString

-- | Routing Type
type RoutingType m a = Sink B.ByteString m a


-- | Run task worker function.
-- the worker check the specific queue in Redis.
-- When get the message, extract and dispatch handler function based on the task name.
--
-- AHandler and BHandler types need to implement Handler type-class.
--
--
-- data MyRouter = AHandlerRoute AHandler
--               | BHandelrRoute BHandler
--               deriving (Eq, Show)
--
-- instance Dispatchable MyRouter where
--     dispatch (AHandlerRoute handler)    = handle handler
--     dispatch (BHandelrRoute handler) = handle handler
--
-- instance FromJSON MyRouter where
--     parseJSON args@(Object v) =
--         case HS.lookup "task" v of
--             Just (String "CheckInventories") ->
--                 CheckInventoriesHandlerRoute <$> (parseJSON args :: Parser CheckInventoriesHandler)
--             Just (String "SearchServiceTask") ->
--                 SearchServiceHandlerRoute <$> (parseJSON args :: Parser SearchServiceHandler)
--             Nothing -> fail "Not Found routing"

-- runApp (routing :: RoutingType IO MyRouter)
--
runApp :: (FromJSON a, Dispatchable a)
       => RoutingType IO a
       -> IO ()
runApp routing = do
    print "Starting worker."
    conn <- connection
    (qm, _) <- redisQSource conn  (B.pack "hsworker")
        =$ decodeUtf8Conduit
        =$ parseBodyConduit
        =$ decodeBase64Conduit
        $$+ CL.take 0
    taskDispatcher routing qm

-- | Specify and use the Router type you specified yourself
routing :: (MonadIO m, FromJSON a, Dispatchable a)
           => RoutingType m a
routing = do
    src <- await
    case src of
        Nothing -> routing
        Just s  -> return $ fromJust (decode (fromStrict s))


-- | configuration of redis
connectInfo :: Redis.ConnectInfo
connectInfo = Redis.defaultConnectInfo
    { Redis.connectHost = "localhost"
    , Redis.connectPort = Redis.PortNumber 6379
    , Redis.connectDatabase = 1
    , Redis.connectMaxConnections = 50
    , Redis.connectMaxIdleTime = 30 }

-- | make redis connection
connection :: IO Redis.Connection
connection = Redis.connect connectInfo

-- | Source function which get the message from redis queue which created by Celery
-- Expect queue type is Redis LIST. this source get the message using LPOP one by one.
-- We will continue to observe even if the queue is empty.
redisQSource :: (Monad m, MonadIO m)
             => Redis.Connection
             -> RedisKey
             -> Source m B.ByteString
redisQSource conn key = do
    src <- liftIO $ Redis.runRedis conn (Redis.lpop key)
    case src of
        Left e  -> redisQSource conn key
        Right m -> case m of
            Nothing -> redisQSource conn key
            Just m' -> yield m' >> redisQSource conn key

-- | Conduit function which convert BytesString to Text.
decodeUtf8Conduit :: Monad m
                  => Conduit B.ByteString m Text
decodeUtf8Conduit = CL.map T.decodeUtf8

-- | Conduit function which parse body from queue message
parseBodyConduit :: Monad m
                 => Conduit Text m Text
parseBodyConduit = CL.map bodyParser
    where
        bodyParser i = fromJust (i ^? key (pack "body") . _String)

-- | Conduit function which convert Base64 to ByteString
-- body contents from queue message was encoded Base64.
-- Because read the body message, we must decode it.
decodeBase64Conduit :: Monad m
                    => Conduit Text m B.ByteString
decodeBase64Conduit = CL.map decodeBase64
    where
        decodeBase64 i = fromRight (Base64.decode $ T.encodeUtf8 i)
        fromRight = either (error . show) id

-- ResumableSource function which dispatch handler function.
-- Dispatch the functions to be executed based on the task name.
taskDispatcher :: (MonadIO m, FromJSON a, Dispatchable a)
           => RoutingType m a
           -> ResumableSource m B.ByteString
           -> m ()
taskDispatcher routing src = do
    (src1, str1) <- src $$++ routing
    liftIO . forkIO $ dispatch str1
    taskDispatcher routing src1
