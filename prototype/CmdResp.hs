{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

import           Control.Applicative

import           FRP.Sodium
import           FRP.Sodium.IO

data Op = Move | Fetch | Ask | PutChar | Done

data OP :: Op -> * where
  MOVE    :: OP Move
  FETCH   :: OP Fetch
  ASK     :: OP Ask
  PUTCHAR :: OP PutChar
  DONE    :: OP Done

data Prompt = AddFeed | Search

data Dir = Up | Down

type family Cmd (o :: Op) :: * where
  Cmd 'Move    = Dir
  Cmd 'Fetch   = ()
  Cmd 'Ask     = Prompt
  Cmd 'PutChar = Char
  Cmd 'Done    = ()

type family Resp (o :: Op) :: * where
  Resp 'Move    = ()
  Resp 'Fetch   = String
  Resp 'Ask     = ()
  Resp 'PutChar = ()
  Resp 'Done    = ()

data ExCmd = forall o. ExCmd (OP o) (Cmd o)

data VtyEvent = Key Char | Enter

eEvent :: Event VtyEvent
eEvent = undefined

data Mode = CommandMode | InputMode

eCmd' :: Reactive (Event (Maybe ExCmd))
eCmd' = collectE cmd CommandMode eEvent
  where
  cmd :: VtyEvent -> Mode -> (Maybe ExCmd, Mode)
  cmd (Key 'j') CommandMode = (Just $ ExCmd MOVE Down,   CommandMode)
  cmd (Key 'k') CommandMode = (Just $ ExCmd MOVE Up,     CommandMode)
  cmd (Key 'u') CommandMode = (Just $ ExCmd FETCH (),    CommandMode)
  cmd (Key 'a') CommandMode = (Just $ ExCmd ASK AddFeed, InputMode)
  cmd _         CommandMode = (Nothing,                  CommandMode)
  cmd Enter     InputMode   = (Just $ ExCmd DONE (),     CommandMode)
  cmd (Key c)   InputMode   = (Just $ ExCmd PUTCHAR c,   InputMode)

eCmd :: Event ExCmd
eCmd = undefined

resp :: OP o -> Cmd o -> IO (Resp o)
resp MOVE  _  = return ()
resp FETCH () = return "apa"
resp ASK   _  = return ()

data ExResp = forall o. ExResp (OP o) (Cmd o) (Resp o)

eResp :: Event ExResp
eResp = executeAsyncIO $ fmap
  (\(ExCmd o p) -> ExResp o p <$> resp o p) eCmd
data Model = Model String

initModel :: Model
initModel = Model ""

rModel :: Reactive (Behaviour Model)
rModel = accum initModel (fmap (\(ExResp o p a) -> update o p a) eResp)
  where
  update :: OP o -> Cmd o -> Resp o -> Model -> Model
  update MOVE  d   () m = m
  update FETCH ()  s  _ = Model s
  update ASK   p  ()  m = m
