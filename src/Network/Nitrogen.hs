module Network.Nitrogen where

import Control.Concurrent
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
  -- Insert, but default to old value.
  let c' = c {status = (insertWith (\_ x -> x) endpoint peerStatus (status c))}
  writeTVar tvarContext c'

addEndpoints :: UTCTime -> [Endpoint] -> TVar Context -> STM ()
addEndpoints expireTime (e:es) tvarContext = do
  addEndpoint expireTime e tvarContext
  addEndpoints expireTime es tvarContext
addEndpoints _ [] _ = return ()

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
  let callerEndpoint = Endpoint (BSChar8.unpack callerAddr) callerPort
  -- Add the new endpoint into our context with an immediate expire time.
  liftIO $ atomically $ addEndpoint now callerEndpoint tvarContext
  knownEndpoints <- liftIO $ atomically . getKnownEndpoints $ tvarContext
  let resp = (logicalTime, knownEndpoints)
  return resp

executeChangesToContext :: Port -> [ Endpoint ] -> TVar Context -> IO ()
executeChangesToContext myPort endpoints tvarContext = do
  let e:es = endpoints
  -- The keys are endpoints for all known clients.
  newLogicalTime <- atomically $ updateTimeFromNewEvent 0 tvarContext
  let req = (newLogicalTime, myPort)
  resp <- request identifyDescriptor e req
  case resp of
    Ok (respLogicalTime, respKnownEndpoints) -> do
      now <- getCurrentTime
      t <- atomically $ do
        t <- updateTimeFromNewEvent respLogicalTime tvarContext
        addEndpoints (addUTCTime 30 now) respKnownEndpoints tvarContext
        return t
      return ()
    _ -> print "Bad RPC call."
  executeChangesToContext myPort es tvarContext

executePerioticProcess :: Int -> TVar Context -> Chan Endpoint -> IO ()
executePerioticProcess delayInSeconds tvarContext chanEndpoint = do
  now <- getCurrentTime
  -- cycle through the Context and find expired endpoints.
  context <- atomically $ readTVar tvarContext
  let expiredEndpoints = findExpiredEndpoints now context
  writeList2Chan chanEndpoint expiredEndpoints
  print (keys . status $ context)
  threadDelay (1000000*delayInSeconds)
  executePerioticProcess delayInSeconds tvarContext chanEndpoint

findExpiredEndpoints :: UTCTime -> Context -> [Endpoint]
findExpiredEndpoints now context = let
  allEndpoints = keys (status context)
  f peerStatus = (expireTime peerStatus) <= now
  in keys $ Data.Map.filter f (status context)
