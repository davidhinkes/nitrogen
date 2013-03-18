import Control.Concurrent.STM.TVar
import Data.Monoid
import qualified Snap.Http.Server.Config as S
import Network.Hstack
import Network.Nitrogen
import Network.Nitrogen.RPC
import System.SimpleArgs

main = do
  (port) <- getArgs
  let config = S.setAccessLog S.ConfigNoLog $ S.setPort port mempty
  context <- newTVarIO $ initialContext
  let h = identifyHandler context
  let r = (registerHandler identifyDescriptor defaultParameters h)
  runWithConfig r config
