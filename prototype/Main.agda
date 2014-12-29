module Main where

open import Category.Functor
open import Category.Monad
open import Data.Unit
open import Data.Container.Indexed
open import Data.Maybe
open import Data.Product
open import IO.Primitive hiding (_>>=_)

open import Signature
open import Sodium
open import Model
open import View

------------------------------------------------------------------------

postulate
  eEvent : Event VtyEvent
  bMode  : Behaviour Mode

eCommand : Event (∃ (Command Hårss))
eCommand = filterJust (snapshot (λ v m → forget (vtyToCmd v m))
                                eEvent bMode)
  where
  forget : ∀ {m} → Maybe (Command Hårss m) → Maybe (∃ (Command Hårss))
  forget nothing  = nothing
  forget (just c) = just (_ , c)

eResponse : Event (∃₂ λ m (c : Command Hårss m) → Response Hårss c)
eResponse = executeAsyncIO (snapshot
  (λ c m → fmap (forget c) (cmdToResp (proj₂ c) m)) eCommand bModel)
  where
  forget : (c : ∃ (Command Hårss)) → Response Hårss (proj₂ c) →
           ∃₂ λ m (c : Command Hårss m) → Response Hårss c
  forget (m , c) r = m , (c , r)

  postulate
    fmap : ∀ {A B} → (A → B) → IO A → IO B
    bModel : Behaviour Model

bModel : Reactive (Behaviour Model)
bModel = accum initialModel (update <$> eResponse)
  where
  open RawFunctor eventFunctor

  update : (∃₂ λ m (c : Command Hårss m) →
           (Response Hårss c)) → Model → Model
  update (._ , moveP d    , _)        m = moveModel d m
  update (._ , promptP p  , _)        m = setMode (input p) m
  update (._ , putCharP c , _)        m = addCharToBuffer c m
  update (._ , doneP , (s , search))  m = searchModel s m
  update (._ , doneP , (s , addFeed)) m = addFeedToModel s m
  update (._ , fetchP     , f)        m = setCurrentFeed m f
  update (._ , searchNextP , s)       m = searchNextModel s m

setup : Reactive (IO ⊤)
setup =
  bModel >>= λ m →
  listen (updates m) view
  where
  open RawMonad reactiveMonad
