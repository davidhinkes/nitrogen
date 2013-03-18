module Network.Nitrogen where

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Map
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BSChar8
import Network.Hstack
import Network.Nitrogen.Types
import Network.Nitrogen.RPC

-- Message defiend with tuples and not data so we get
-- Serial for free.
type Message t = (LogicalTime, t)

messageLogicalTime :: Message t -> LogicalTime
messageLogicalTime (x, _) = x

messageContent :: Message t -> t
messageContent (_, x) = x

data PeerStatus = PeerStatus {
  expireTime :: UTCTime
  --  outoingMessages :: [ Message t ]
}

data Context = Context {
  currentLogicalTime :: LogicalTime,
  status :: Map Endpoint PeerStatus
}

initialContext :: Context
initialContext = Context 0 empty

-- Notify the Context that a new event has occured.
updateTimeFromNewEvent :: LogicalTime -> TVar Context -> STM LogicalTime
updateTimeFromNewEvent existingTime tvarContext = do
  context <- readTVar tvarContext
  let previousTime = currentLogicalTime context
  let newTime = max (previousTime + 1) (existingTime + 1)
  let newContext = context { currentLogicalTime = newTime }
  writeTVar tvarContext newContext
  return newTime

addEndpoint :: UTCTime -> Endpoint -> TVar Context -> STM ()
addEndpoint expireTime endpoint tvarContext = do
  c <- readTVar tvarContext
  let peerStatus = PeerStatus expireTime
  let c' = c {status = (insert endpoint peerStatus (status c))}
  writeTVar tvarContext c'

getKnownEndpoints :: TVar Context -> STM [Endpoint]
getKnownEndpoints tvarContext = do
  c <- readTVar tvarContext
  return (keys $ status c)

identifyHandler :: TVar Context -> Handler IO IdentifyRequest IdentifyResponse
identifyHandler tvarContext = do
  (callerLogicalTime, callerPort) <- getInput
  logicalTime <- liftIO $ atomically $ updateTimeFromNewEvent callerLogicalTime tvarContext
  callerAddr <- getRemoteAddr
  now <- liftIO getCurrentTime
  -- Add the new endpoint into our context with an immediate expire time.
  let expireTime = addUTCTime (fromInteger 0) now
  let callerEndpoint = Endpoint (BSChar8.unpack callerAddr) callerPort
  liftIO $ atomically $ addEndpoint expireTime callerEndpoint tvarContext
  knownEndpoints <- liftIO $ atomically . getKnownEndpoints $ tvarContext
  let resp = (logicalTime, knownEndpoints)
  return resp
