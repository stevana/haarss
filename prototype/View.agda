module View where

open import Data.Unit
open import Data.Char
open import Data.Maybe
open import Data.Container.Indexed

open import Signature
open import Model
open import IO.Primitive

------------------------------------------------------------------------

postulate
  view : Model → IO ⊤

data VtyEvent : Set where
  key   : Char → VtyEvent
  enter : VtyEvent

vtyToCmd : VtyEvent → (m : Mode) → Maybe (Command Hårss m)
vtyToCmd (key 'j')  cmd            = just (moveP down)
vtyToCmd (key 'k')  cmd            = just (moveP up)
vtyToCmd (key 'a')  cmd            = just (promptP addFeed)
vtyToCmd (key '\t') cmd            = just (promptP search)
vtyToCmd (key 'R')  cmd            = just fetchP
vtyToCmd _          cmd            = nothing
vtyToCmd enter      (input p)      = just doneP
vtyToCmd (key '\t') (input search) = just searchNextP
vtyToCmd (key c)    (input p)      = just (putCharP c)
