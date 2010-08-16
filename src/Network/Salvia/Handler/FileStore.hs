{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Network.Salvia.Handler.FileStore
( Router (..)
, defaultRouter
, hFileStore
, hFileStoreFile
, hDir
, hIndex
, hSearch
, hRetrieve
, hLatest
, hSave
, hDelete
, hHistory
)
where

import Control.Exception
import Control.Monad.Trans
import Data.FileStore
import Data.List (intercalate)
import Data.List.Split
import Data.Record.Label
import Network.Protocol.Http hiding (NotFound)
import Network.Protocol.Uri
import Network.Salvia.Handlers
import Network.Salvia.Interface
import qualified Network.Protocol.Http as Http

data Router m = Router
  { rtIndex     ::                                      m ()
  , rtDirectory :: String                            -> m ()
  , rtSearch    :: String                            -> m ()
  , rtHistory   :: FilePath                          -> m ()
  , rtLatest    :: FilePath                          -> m ()
  , rtRetrieve  :: FilePath -> String                -> m ()
  , rtSave      :: FilePath -> Description -> Author -> m ()
  , rtDelete    :: FilePath -> Description -> Author -> m ()
  , rtError     :: m ()
  }

defaultRouter :: (MonadIO m, BodyM Request m, HttpM' m, SendM m) => FileStore -> Router m
defaultRouter fs = Router
  { rtIndex     = hIndex fs
  , rtDirectory = hDir fs
  , rtSearch    = hSearch fs
  , rtHistory   = hHistory fs
  , rtLatest    = hLatest fs
  , rtRetrieve  = hRetrieve fs
  , rtSave      = hSave fs
  , rtDelete    = hDelete fs
  , rtError     = hError Http.NotFound
  }

-- Top level filestore server.

hFileStore
  :: (MonadIO m, BodyM Request m, HttpM' m, SendM m)
  => Router m -> Author -> FilePath -> m ()
hFileStore rt author = hFileTypeDispatcher (rtDirectory rt) (hFileStoreFile rt author)

hFileStoreFile
  :: (MonadIO m, BodyM Request m, HttpM' m, SendM m)
  => Router m -> Author -> FilePath -> m ()
hFileStoreFile rt author _ =
  do m <- request (getM method)
     u <- request (getM asUri)
     let p = mkRelative (getL path u)
         q = getL query u

     -- Default content type to text/plain, and override in hLatest and
     -- hRetrieve.
     response (contentType =: Just ("text/plain", Nothing))

     -- REST based routing.
     case (p, m, q) of
       ("index",  GET,    _        ) -> rtIndex     rt
       ("search", GET,    _        ) -> rtSearch    rt q
       (_,        GET,    "history") -> rtHistory   rt p
       (_,        GET,    "latest" ) -> rtLatest    rt p
       (_,        GET,    _        ) -> rtRetrieve  rt p q
       (_,        PUT,    _        ) -> rtSave      rt p q author
       (_,        DELETE, _        ) -> rtDelete    rt p q author
       _                             -> hError Http.NotFound

-- Type class alias.

class    (MonadIO m, BodyM Request m, HttpM' m, SendM m) => F m where
instance (MonadIO m, BodyM Request m, HttpM' m, SendM m) => F m

-- Specific filestore operations.

hDir :: F m => FileStore -> FilePath -> m ()
hDir fs _ =
  do u <- request (getM asUri)
     let p = mkRelative (getL path u)
     run (directory fs p) (intercalate "\n" . map showFS)
  where showFS (FSFile      f) = f
        showFS (FSDirectory d) = d ++ "/"

hIndex :: F m => FileStore -> m ()
hIndex fs = run (index fs) (intercalate "\n")

hSearch :: F m => FileStore -> String -> m ()
hSearch fs q =
  run (search fs sq) showMatches
  where showMatches = intercalate "\n" . map showMatch
        showMatch (SearchMatch f n l) = intercalate ":" [f, show n, l]
        sq = defaultSearchQuery
               { queryMatchAll   = False
               , queryWholeWords = False
               , queryIgnoreCase = False
               , queryPatterns   = splitOn "&" q
               }

hRetrieve :: F m => FileStore -> FilePath -> String -> m ()
hRetrieve fs p q =
  do response (contentType =: Just (fileMime p, Nothing))
     run (retrieve fs p (if null q then Nothing else Just q)) id

hLatest :: F m => FileStore -> FilePath -> m ()
hLatest fs p =
  do response (contentType =: Just (fileMime p, Nothing))
     run (latest fs p) id

hSave :: F m => FileStore -> FilePath -> Description -> Author -> m ()
hSave fs p q author =
  do b <- hRawRequestBody
     run (save fs p author q b) (const "document saved\n")

hDelete :: F m => FileStore -> FilePath -> Description -> Author -> m ()
hDelete fs p q author = run (delete fs p author q) (const "document deleted\n")

hHistory :: (SendM m, MonadIO m, BodyM Request m, HttpM' m) => FileStore -> FilePath -> m ()
hHistory fs p = getHistory fs p >>= either fsError (send . showHistory) 

getHistory :: (Exception e, F m) => FileStore -> FilePath -> m (Either e [Revision])
getHistory fs p = (liftIO . try) (history fs [p] (TimeRange Nothing Nothing))

showHistory :: [Revision] -> String
showHistory = intercalate "\n" . map showRevision
  where showRevision (Revision i d a s _) = intercalate "," [i, show d, showAuthor a, s]
        showAuthor (Author n e) = n ++ " <" ++ e ++ ">"

-- Helper functions.

run :: F m => IO a -> (a -> String) -> m ()
run action f =
  do e <- liftIO (try action)
     case e of
       Left err  -> hCustomError (mkError err) (show err)
       Right res -> send (f res)

fsError :: (HttpM Response m, SendM m) => FileStoreError -> m ()
fsError err = hCustomError (mkError err) (show err)

mkError :: FileStoreError -> Status
mkError RepositoryExists     = Http.BadRequest
mkError ResourceExists       = Http.BadRequest
mkError NotFound             = Http.NotFound
mkError IllegalResourceName  = Http.NotFound
mkError Unchanged            = Http.OK
mkError UnsupportedOperation = Http.BadRequest
mkError NoMaxCount           = Http.InternalServerError
mkError _                    = Http.BadRequest

mkRelative :: String -> String
mkRelative = dropWhile (=='/')

