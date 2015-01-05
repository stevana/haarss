{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}

module Interface where

import Data.Text (Text)
import Data.Time
import Test.QuickCheck

import Feed.Annotated

data Op'
  = Move'
  | MarkAsRead'
  | MarkAllAsRead'
  | UpdateFeed'
  | UpdateFeeds'
  | OpenUrl'
  | OpenPrompt'
  | PutPrompt'
  | DelPrompt'
  | CancelPrompt'
  | ClosePrompt'
  | Search'
  | Resize'
  | Quit'

data Op :: Op' -> * where
  Move           :: Op Move'
  MarkAsRead     :: Op MarkAsRead'
  MarkAllAsRead  :: Op MarkAllAsRead'
  UpdateFeed     :: Op UpdateFeed'
  UpdateFeeds    :: Op UpdateFeeds'
  OpenUrl        :: Op OpenUrl'
  OpenPrompt     :: Op OpenPrompt'
  PutPrompt      :: Op PutPrompt'
  DelPrompt      :: Op DelPrompt'
  CancelPrompt   :: Op CancelPrompt'
  ClosePrompt    :: Op ClosePrompt'
  Search         :: Op Search'
  Resize         :: Op Resize'
  Quit           :: Op Quit'

data Dir = Up | Down | In | Out | Top | Bot
  deriving (Show, Eq, Enum)

instance Arbitrary Dir where
  arbitrary = elements [Up, Down, In , Out, Top, Bot]

data Prompt = AddFeed | SearchPrompt
  deriving Eq

instance Show Prompt where
  show AddFeed      = "Add feed"
  show SearchPrompt = "Search for"

instance Arbitrary Prompt where
  arbitrary = elements [AddFeed, SearchPrompt]

type family Cmd (o :: Op') :: * where
  Cmd 'Move'           = Dir
  Cmd 'MarkAsRead'     = ()
  Cmd 'MarkAllAsRead'  = ()
  Cmd 'UpdateFeed'     = Maybe String
  Cmd 'UpdateFeeds'    = [String]
  Cmd 'OpenUrl'        = Maybe Text
  Cmd 'OpenPrompt'     = Prompt
  Cmd 'PutPrompt'      = Char
  Cmd 'DelPrompt'      = ()
  Cmd 'CancelPrompt'   = ()
  Cmd 'ClosePrompt'    = ()
  Cmd 'Search'         = ()
  Cmd 'Resize'         = ()
  Cmd 'Quit'           = [AnnFeed]

data ExCmd = forall o. ExCmd (Op o) (Cmd o)

type family Resp (o :: Op') :: * where
  Resp 'Move'           = ()
  Resp 'MarkAsRead'     = ()
  Resp 'MarkAllAsRead'  = ()
  Resp 'UpdateFeed'     = Maybe AnnFeed
  Resp 'UpdateFeeds'    = (UTCTime, [AnnFeed])
  Resp 'OpenUrl'        = ()
  Resp 'OpenPrompt'     = ()
  Resp 'PutPrompt'      = ()
  Resp 'DelPrompt'      = ()
  Resp 'CancelPrompt'   = ()
  Resp 'ClosePrompt'    = ()
  Resp 'Search'         = ()
  Resp 'Resize'         = ()
  Resp 'Quit'           = ()

data ExResp = forall o. ExResp (Op o) (Cmd o) (Resp o)


data Mode = Normal | Input

data Feedback = FeedDownloaded | Downloading Int
