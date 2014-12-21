{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fetching.History where

import Control.Monad
import Data.Serialize
import Data.Time (UTCTime(..), DiffTime, Day(..), toModifiedJulianDay)
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Status (Status(..))

------------------------------------------------------------------------

data History
  = Success UTCTime
  | Failure UTCTime FailureReason
  deriving Generic

data FailureReason
  = DownloadFailure HttpExceptionSimple
  | ParseFailure String
  | TimeoutFailure
  | UnknownFailure
  deriving Generic

data HttpExceptionSimple
  = StatusCodeException' Status
  | OtherException
  deriving Generic

simplifyHttpException :: HttpException -> HttpExceptionSimple
simplifyHttpException e = case e of
  StatusCodeException s _ _          -> StatusCodeException' s
  InvalidUrlException _ _            -> OtherException
  TooManyRedirects _                 -> OtherException
  UnparseableRedirect _              -> OtherException
  TooManyRetries                     -> OtherException
  HttpParserException _              -> OtherException
  HandshakeFailed                    -> OtherException
  OverlongHeaders                    -> OtherException
  ResponseTimeout                    -> OtherException
  FailedConnectionException _ _      -> OtherException
  FailedConnectionException2 _ _ _ _ -> OtherException
  ExpectedBlankAfter100Continue      -> OtherException
  InvalidStatusLine _                -> OtherException
  InvalidHeader _                    -> OtherException
  InternalIOException _              -> OtherException
  ProxyConnectException _ _ _        -> OtherException
  NoResponseDataReceived             -> OtherException
  TlsException _                     -> OtherException
  TlsNotSupported                    -> OtherException
  ResponseBodyTooShort _ _           -> OtherException
  InvalidChunkHeaders                -> OtherException
  IncompleteHeaders                  -> OtherException
  InvalidDestinationHost _           -> OtherException
  HttpZlibException _                -> OtherException

failed :: [History] -> Bool
failed (Failure _ _ : _) = True
failed _                 = False

------------------------------------------------------------------------

-- Discussion about how to serialise UTCTime:
-- https://www.haskell.org/pipermail/haskell-cafe/2012-January/098683.html

instance Serialize UTCTime where
  get                    = liftM2 UTCTime get get
  put (UTCTime day time) = put day >> put time

instance Serialize Day where
  get = liftM ModifiedJulianDay get
  put = put . toModifiedJulianDay

instance Serialize DiffTime where
  get = liftM fromRational get
  put = put . toRational


instance Serialize HttpExceptionSimple where
instance Serialize History             where
instance Serialize FailureReason       where

deriving instance Generic Status
instance Serialize Status              where
