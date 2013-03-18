module Network.Nitrogen.RPC  where

import Network.Hstack
import Network.Nitrogen.Types

type Port = Integer

type IdentifyRequest = (LogicalTime, Port)

type IdentifyResponse = (LogicalTime, [Endpoint])

identifyDescriptor :: ServiceDescriptor IdentifyRequest IdentifyResponse
identifyDescriptor = ServiceDescriptor "identify"
