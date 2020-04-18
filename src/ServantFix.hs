{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module ServantFix where

import Data.Reflection
import GHC.TypeLits
import Servant

prefix :: KnownSymbol s => Proxy s -> Proxy api -> Proxy (s :> api)
prefix Proxy Proxy = Proxy

prefixing ::
     String
  -> Proxy api
  -> (forall s. KnownSymbol s =>
                  Proxy (s :> api) -> r)
  -> r
prefixing s api f = reifySymbol s $ \sProxy -> f (prefix sProxy api)

serveUnder ::
     HasServer api '[] => String -> Proxy api -> Server api -> Application
serveUnder prefix api server =
  prefixing prefix api $ \apiProxy -> serve apiProxy server

serveUnderWithContext ::
     HasServer api ctx
  => String
  -> Proxy api
  -> Context ctx
  -> Server api
  -> Application
serveUnderWithContext prefix api ctx server =
  prefixing prefix api $ \apiProxy -> serveWithContext apiProxy ctx server
