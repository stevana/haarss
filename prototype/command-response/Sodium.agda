module Sodium where

open import Category.Functor
open import Category.Applicative
open import Category.Monad
open import Data.Unit
open import Data.Bool
open import Data.Maybe
open import Data.Product
open import IO.Primitive

postulate

  -- Core.

  Reactive : Set → Set
  sync     : ∀ {A} → Reactive A → IO A

  Event     : Set → Set
  Behaviour : Set → Set

  listen    : ∀ {A} → Event A → (A → IO ⊤) → Reactive (IO ⊤)

  filterJust    : ∀ {A} → Event (Maybe A) → Event A
  hold          : ∀ {A} → A → Event A → Reactive (Behaviour A)
  updates value : ∀ {A} → Behaviour A → Event A
  snapshot      : ∀ {A B C} → (A → B → C) → Event A → Behaviour B → Event C

  -- Derived.

  gate     : ∀ {A} → Event A → Behaviour Bool → Event A
  filterE  : ∀ {A} → (A → Bool) → Event A → Event A

  collectE : ∀ {A B} {S : Set} → (A → S → (B × S)) → S →
             Event A → Reactive (Event B)

  collect  : ∀ {A B} {S : Set} → (A → S → (B × S)) → S →
             Behaviour A → Reactive (Behaviour B)

  accum    : ∀ {A} → A → Event (A → A) → Reactive (Behaviour A)

  -- IO.

  executeSyncIO executeAsyncIO : ∀ {A} → Event (IO A) → Event A

  -- Instances.

  reactiveMonad        : RawMonad Reactive
  behaviourApplicative : RawApplicative Behaviour
  eventFunctor         : RawFunctor Event
