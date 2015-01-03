{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}

module Interface where

import Data.Text (Text)
import Data.Time

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
  ClosePrompt    :: Op ClosePrompt'
  Search         :: Op Search'
  Resize         :: Op Resize'
  Quit           :: Op Quit'

data Dir = Up | Down | In | Out | Top | Bot
  deriving (Show, Eq, Enum)

data Prompt = AddFeed | SearchPrompt

type family Cmd (o :: Op') :: * where
  Cmd 'Move'           = Dir
  Cmd 'MarkAsRead'     = ()
  Cmd 'MarkAllAsRead'  = ()
  Cmd 'UpdateFeed'     = Maybe String
  Cmd 'UpdateFeeds'    = [String]
  Cmd 'OpenUrl'        = Maybe Text
  Cmd 'OpenPrompt'     = Prompt
  Cmd 'PutPrompt'      = Char
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
  Resp 'ClosePrompt'    = ()
  Resp 'Search'         = ()
  Resp 'Resize'         = ()
  Resp 'Quit'           = ()

data ExResp = forall o. ExResp (Op o) (Cmd o) (Resp o)


data Mode = Normal | Input

data Feedback = FeedDownloaded | Downloading Int
