{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import FRP.Sodium
import FRP.Sodium.IO

data Op = Move | Fetch

data Dir = Up | Down

data Cmd (o :: Op) where
  MoveP  :: Dir -> Cmd Move
  FetchP :: Cmd Fetch

data Resp (o :: Op) where
  MoveA  :: Resp Move
  FetchA :: String -> Resp Fetch

data VtyEvent = Key Char | Enter

eEvent :: Event VtyEvent
eEvent = undefined

data Ex (p :: k -> *) where
  Ex :: p i -> Ex p

eCmd :: Event (Ex Cmd)
eCmd = filterJust $ fmap cmd eEvent
  where
  cmd :: VtyEvent -> Maybe (Ex Cmd)
  cmd (Key 'j') = Just $ Ex (MoveP Down)
  cmd (Key 'k') = Just $ Ex (MoveP Up)
  cmd (Key 'u') = Just $ Ex FetchP
  cmd _         = Nothing

resp :: Cmd o -> IO (Resp o)
resp (MoveP _) = return MoveA
resp FetchP    = return (FetchA "apa")

data (:*) (p :: k -> *) (q :: k -> *) :: k -> * where
  (:&) :: p i -> q i -> (p :* q) i

eResp :: Event (Ex (Cmd :* Resp))
eResp = executeAsyncIO $ fmap (\(Ex p) -> do
  a <- resp p
  return (Ex (p :& a))) eCmd

data Model = Model String

initModel :: Model
initModel = Model ""

rModel :: Reactive (Behaviour Model)
rModel = accum initModel (fmap (\(Ex (p :& a)) -> update p a) eResp)
  where
  update :: Cmd o -> Resp o -> Model -> Model
  update (MoveP _)    MoveA     m = m
  update  FetchP     (FetchA s) _ = Model s
  update _           _          _ = error "Impossible."
