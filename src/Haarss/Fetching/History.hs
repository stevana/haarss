{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haarss.Fetching.History where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Serialize
import           Data.Time                 (Day (..), DiffTime, UTCTime (..),
                                            secondsToDiffTime,
                                            toModifiedJulianDay)
import           GHC.Generics              (Generic)
import           Network.HTTP.Client       (HttpException (..))
import           Network.HTTP.Types.Status (Status (..))
import           Test.QuickCheck           hiding (Failure, Success)

------------------------------------------------------------------------

data History
  = Success UTCTime
  | Failure UTCTime FailureReason
  deriving (Eq, Show, Generic)

data FailureReason
  = DownloadFailure HttpExceptionSimple
  | ParseFailure String
  | TimeoutFailure
  | UnknownFailure
  deriving (Eq, Generic)

instance Show FailureReason where
  show (DownloadFailure (StatusCodeException' s))
    = "Status code: " ++ show (statusCode s) ++ "."
  show (DownloadFailure _)
    = "Unknown download failure."
  show (ParseFailure e) = "Parse failure (" ++ e ++ ")"
  show TimeoutFailure   = "Timeout failure."
  show UnknownFailure   = "Unknown failure."

data HttpExceptionSimple
  = StatusCodeException' Status
  | OtherException
  deriving (Eq, Show, Generic)

makePrisms ''History

simplifyHttpException :: HttpException -> HttpExceptionSimple
simplifyHttpException e = case e of
  StatusCodeException s _ _            -> StatusCodeException' s
  InvalidUrlException _ _              -> OtherException
  TooManyRedirects _                   -> OtherException
  UnparseableRedirect _                -> OtherException
  TooManyRetries                       -> OtherException
  HttpParserException _                -> OtherException
  HandshakeFailed                      -> OtherException
  OverlongHeaders                      -> OtherException
  ResponseTimeout                      -> OtherException
  FailedConnectionException _ _        -> OtherException
  FailedConnectionException2 {}        -> OtherException
  ExpectedBlankAfter100Continue        -> OtherException
  InvalidStatusLine _                  -> OtherException
  InvalidHeader _                      -> OtherException
  InternalIOException _                -> OtherException
  ProxyConnectException {}             -> OtherException
  NoResponseDataReceived               -> OtherException
  TlsException _                       -> OtherException
  TlsNotSupported                      -> OtherException
  ResponseBodyTooShort _ _             -> OtherException
  InvalidChunkHeaders                  -> OtherException
  IncompleteHeaders                    -> OtherException
  InvalidDestinationHost _             -> OtherException
  HttpZlibException _                  -> OtherException
  InvalidProxyEnvironmentVariable {}   -> OtherException
  ResponseLengthAndChunkingBothUsed {} -> OtherException

failed :: [History] -> Bool
failed (Failure _ _ : _) = True
failed _                 = False

------------------------------------------------------------------------

-- Discussion about how to serialise UTCTime:
-- https://www.haskell.org/pipermail/haskell-cafe/2012-January/098683.html

instance Serialize UTCTime where
  get                    = UTCTime <$> get <*> get
  put (UTCTime day time) = put day >> put time

instance Serialize Day where
  get = ModifiedJulianDay <$> get
  put = put . toModifiedJulianDay

instance Serialize DiffTime where
  get = fromRational <$> get
  put = put . toRational

instance Serialize HttpExceptionSimple where
instance Serialize History             where
instance Serialize FailureReason       where

deriving instance Generic Status
instance Serialize Status              where

instance Arbitrary History where
  arbitrary = oneof
    [ Success <$> arbitrary
    , Failure <$> arbitrary <*> arbitrary
    ]

instance Arbitrary FailureReason where
  arbitrary = oneof
    [ -- DownloadFailure HttpExceptionSimple
      ParseFailure <$> arbitrary
    , pure TimeoutFailure
    , pure UnknownFailure
    ]

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
  arbitrary = secondsToDiffTime <$> arbitrary

instance NFData History where
