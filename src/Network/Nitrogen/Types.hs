module Network.Nitrogen.Types where

import Data.Word
import Data.Serialize
import Network.Hstack

type LogicalTime = Word64

-- TODO: add unit tests for seralizing Endpoints.
instance Serialize Endpoint where
  put a = do
    put $ host a
    put $ port a
  get = do
    host <- get
    port <- get
    return $ Endpoint host port 

instance Show Endpoint where
  show (Endpoint h p) = show h ++ ":" ++ show p
