import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Map
import Data.Monoid
import Data.Time.Clock
import qualified Snap.Http.Server.Config as S
import Network.Hstack
import Network.Nitrogen
import Network.Nitrogen.RPC
import System.SimpleArgs

main = do
  (port, bootstrapHost, bootstrapPort) <- getArgs
  let config = S.setAccessLog S.ConfigNoLog $ S.setPort port mempty
  let bootstrapEndpoint = Endpoint bootstrapHost bootstrapPort
  now <- getCurrentTime
  context <- newTVarIO $ initialContext
  -- Add the bootstrap enpoint with an expire time of now.
  atomically $ addEndpoint now bootstrapEndpoint context
  chan <- newChan
  chanList <- getChanContents chan
  let portAsPortType = fromInteger . toInteger $ port
  -- Start a new thread responsible for making backend requests
  -- and updating the context.
  forkIO $ executeChangesToContext portAsPortType chanList context
  forkIO $ executePerioticProcess 5 context chan
  let h = identifyHandler context
  let r = (registerHandler identifyDescriptor defaultParameters h)
  runWithConfig r config

