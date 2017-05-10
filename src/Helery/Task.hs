module Helery.Task
    ( Handler
    , BaseHandler (BaseHandler)
    , Dispatchable
    , handle
    , dispatch
    ) where

import           Data.Aeson      (FromJSON)
import           Data.Text       (Text)
import           Data.UUID.Aeson ()
import           GHC.Generics    (Generic)


-- | implement task handler action
class Handler a where
    handle :: a -> IO ()

-- | implement task routing
class Dispatchable a where
    dispatch :: a -> IO ()

data BaseHandler a b = BaseHandler
    { task   :: Text
    , args   :: a
    , kwargs :: b
    } deriving (Eq, Show)
