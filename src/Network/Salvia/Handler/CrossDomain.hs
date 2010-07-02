{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Network.Salvia.Handler.CrossDomain
(

-- * Cross domain XmlHttpRequests handlers.

  hCrossDomain
, hCrossDomainAny

-- * Custom request and response headers specialized for cross domain requests.

, accessControlAllowOrigin
, accessControlRequestMethod
, accessControlRequestHeaders
, accessControlAllowMethods
, accessControlAllowHeaders
, accessControlMaxAge
)
where

import Control.Applicative
import Data.List
import Data.Record.Label
import Network.Salvia

-- | Handler to ensure browser some specific domains may perform cross-domain
-- requests to this handler. See the `Cross-Origin Resource Sharing' working
-- draft at http://www.w3.org/TR/cors/.

hCrossDomain
  :: HttpM' m
  => (String -> Bool)  -- ^ A function to validate the origin domain.
  -> [Method]          -- ^ A list of allowed HTTP request methods.
  -> Integer           -- ^ The number of seconds this response remains valid.
  -> m ()              -- ^ A handler to run when no access is granted.
  -> m ()              -- ^ The fall through handler.
  -> m ()
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
           Just o                    -> h <* setHeaders (Just o)
           _                         -> h

    setHeaders orig = response $
      do accessControlAllowOrigin  =: orig
         accessControlAllowMethods =: Just (intercalate ", " (map show meths))
         accessControlMaxAge       =: Just (show maxAge)

-- | Like `hCrossDomain' but allow all hosts and timeout in one day exactly.

hCrossDomainAny :: (HttpM' m, SendM m) => m () -> m ()
hCrossDomainAny = hCrossDomain (const True) methods (3600 * 24) (hError Forbidden)

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

