module Network.Nitrogen.RPC  where

import Data.Word
import Network.Hstack
import Network.Nitrogen.Types

type IdentifyRequest = (LogicalTime, Port)

type IdentifyResponse = (LogicalTime, [Endpoint])

identifyDescriptor :: ServiceDescriptor IdentifyRequest IdentifyResponse
identifyDescriptor = ServiceDescriptor "identify"
