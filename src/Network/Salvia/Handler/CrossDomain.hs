{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Network.Salvia.Handler.CrossDomain
( hCrossDomainAny
, hCrossDomain
, accessControlAllowOrigin
, accessControlRequestMethod
, accessControlRequestHeaders
, accessControlAllowMethods
, accessControlAllowHeaders
, accessControlMaxAge
)
where

import Data.List
import Data.Record.Label
import Network.Salvia

hCrossDomainAny :: (HttpM' m, SendM m) => m () -> m ()
hCrossDomainAny = hCrossDomain (const True) methods (3600 * 24) (hError Forbidden)

hCrossDomain
  :: HttpM' m
  => (String -> Bool) -> [Method] -> Integer -> m () -> m () -> m ()
hCrossDomain validate meths maxAge onerr =
  hMethod OPTIONS indirect . direct
  where
    indirect =
      do meth <- request (getM accessControlRequestMethod)
         orig <- request (getM origin)
         if maybe False validate orig && meth `elem` meths
           then setHeaders orig
           else onerr

    direct h =
      do orig <- request (getM origin)
         case orig of
           Just o | not (validate o) -> onerr
           _                         -> h

    setHeaders orig = response $
      do accessControlAllowOrigin  =: orig
         accessControlAllowMethods =: Just (intercalate ", " (map show meths))
         accessControlMaxAge       =: Just (show maxAge)

-- | Custom request and response headers specialized for cross domain requests.

accessControlAllowOrigin :: Http a :-> Maybe Value
accessControlAllowOrigin = header "Access-Control-Allow-Origin"

accessControlRequestMethod :: Http a :-> Method
accessControlRequestMethod =
  (maybe GET methodFromString :<->: Just . show)
    % header "Access-Control-Request-Method"

accessControlRequestHeaders :: Http a :-> Maybe Value
accessControlRequestHeaders = header "Access-Control-Request-Headers"

accessControlAllowMethods :: Http a :-> Maybe Value
accessControlAllowMethods = header "Access-Control-Allow-Methods"

accessControlAllowHeaders :: Http a :-> Maybe Value
accessControlAllowHeaders = header "Access-Control-Allow-Headers"

accessControlMaxAge :: Http a :-> Maybe Value
accessControlMaxAge = header "Access-Control-Max-Age"

