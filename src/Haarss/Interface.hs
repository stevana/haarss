{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}

module Haarss.Interface where

import           Data.Time
import           Test.QuickCheck

import           Haarss.Feed.Annotated

------------------------------------------------------------------------

data Op'
  = Move'
  | MarkAsRead'
  | MarkAllAsRead'
  | UpdateFeeds'
  | OpenUrl'
  | OpenPrompt'
  | PutPrompt'
  | DelPrompt'
  | CancelPrompt'
  | ClosePrompt'
  | RemoveFeed'
  | Rearrange'
  | Search'
  | Scroll'
  | Resize'
  | Quit'

data Op :: Op' -> * where
  Move           :: Op Move'
  MarkAsRead     :: Op MarkAsRead'
  MarkAllAsRead  :: Op MarkAllAsRead'
  UpdateFeeds    :: Op UpdateFeeds'
  OpenUrl        :: Op OpenUrl'
  OpenPrompt     :: Op OpenPrompt'
  PutPrompt      :: Op PutPrompt'
  DelPrompt      :: Op DelPrompt'
  CancelPrompt   :: Op CancelPrompt'
  ClosePrompt    :: Op ClosePrompt'
  RemoveFeed     :: Op RemoveFeed'
  Rearrange      :: Op Rearrange'
  Search         :: Op Search'
  Scroll         :: Op Scroll'
  Resize         :: Op Resize'
  Quit           :: Op Quit'

data Dir = Up | Down | In | Out | Top | Bot
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Dir where
  arbitrary = elements [minBound ..]

data Prompt = AddFeed | RenameFeed | SearchPrompt
  deriving Eq

instance Show Prompt where
  show AddFeed      = "Add feed"
  show RenameFeed   = "Rename feed to"
  show SearchPrompt = "Search for"

instance Arbitrary Prompt where
  arbitrary = elements [AddFeed, SearchPrompt]

data ScrollDir = DownFull | DownHalf | UpFull | UpHalf
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary ScrollDir where
  arbitrary = elements [minBound ..]

type family Cmd (o :: Op') :: * where
  Cmd 'Move'           = Dir
  Cmd 'MarkAsRead'     = ()
  Cmd 'MarkAllAsRead'  = ()
  Cmd 'UpdateFeeds'    = [String]
  Cmd 'OpenUrl'        = Maybe String
  Cmd 'OpenPrompt'     = Prompt
  Cmd 'PutPrompt'      = Char
  Cmd 'DelPrompt'      = ()
  Cmd 'CancelPrompt'   = ()
  Cmd 'ClosePrompt'    = ()
  Cmd 'RemoveFeed'     = ()
  Cmd 'Rearrange'      = Dir
  Cmd 'Search'         = ()
  Cmd 'Scroll'         = ScrollDir
  Cmd 'Resize'         = ()
  Cmd 'Quit'           = [AnnFeed]

data ExCmd = forall o. ExCmd (Op o) (Cmd o)

type family Resp (o :: Op') :: * where
  Resp 'Move'           = ()
  Resp 'MarkAsRead'     = ()
  Resp 'MarkAllAsRead'  = ()
  Resp 'UpdateFeeds'    = (UTCTime, [AnnFeed])
  Resp 'OpenUrl'        = ()
  Resp 'OpenPrompt'     = ()
  Resp 'PutPrompt'      = ()
  Resp 'DelPrompt'      = ()
  Resp 'CancelPrompt'   = ()
  Resp 'ClosePrompt'    = ()
  Resp 'RemoveFeed'     = ()
  Resp 'Rearrange'      = ()
  Resp 'Search'         = ()
  Resp 'Scroll'         = ()
  Resp 'Resize'         = ()
  Resp 'Quit'           = ()

data ExResp = forall o. ExResp (Op o) (Cmd o) (Resp o)


data Mode = Normal | Input

data Feedback = FeedDownloaded | Downloading Int
