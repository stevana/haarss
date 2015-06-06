module Signature where

open import Function renaming (const to K)
open import Category.Monad.Predicate
open import Data.Sum
open import Data.Product
open import Data.Container.Indexed hiding (_∈_)
open import Data.Container.Indexed.Combinator
  renaming (_⊎_ to _⊎^C_; _×_ to _×^C_)
open import Data.Container.Indexed.FreeMonad
open import Data.Char
open import Data.String
open import Relation.Unary
open import Relation.Binary.PropositionalEquality

------------------------------------------------------------------------

data Prompt : Set where
  addFeed search : Prompt

data Mode : Set where
  cmd   : Mode
  input : Prompt → Mode

data Dir : Set where
  up down : Dir

postulate
  Feed : Set

Hårss : Mode ▷ Mode
Hårss =
  {- move    -} ｛ cmd ｝ ∩ K Dir ◃
                U /
                (λ _ _ → cmd) ⊎^C
  {- prompt  -} ｛ cmd ｝ ∩ K Prompt ◃
                U /
                (λ { (_ , p) _ → input p}) ⊎^C
  {- putChar -} (⋃[ p ∶ Prompt ] ｛ input p ｝) ∩ K Char ◃
                U /
                (λ { {._} ((p , refl) , _) _ → input p }) ⊎^C
  {- done    -} ⋃[ p ∶ Prompt ] ｛ input p ｝ ◃
                K (String × Prompt) /
                (λ _ _ → cmd) ⊎^C
  {- fetch   -} ｛ cmd ｝ ◃
                K Feed /
                (λ _ _ → cmd) ⊎^C
  {- searchN -} ｛ input search ｝ ◃
                K String /
                (λ _ _ → input search)


pattern moveP d = inj₁ (refl , d)

move : Dir → cmd ∈ Hårss ⋆ (U ∩ ｛ cmd ｝)
move d = generic (moveP d)

pattern promptP p = inj₂ (inj₁ (refl , p))

prompt : (p : Prompt) → cmd ∈ Hårss ⋆ (U ∩ ｛ input p ｝)
prompt p = generic (promptP p)

pattern putCharP c = inj₂ (inj₂ (inj₁ ((_ , refl) , c)))

putChar : ∀ {p} → Char → input p ∈ Hårss ⋆ (U ∩ ｛ input p ｝)
putChar c = generic (putCharP c)

pattern doneP = inj₂ (inj₂ (inj₂ (inj₁ (_ , refl))))

done : ∀ {p} → input p ∈ Hårss ⋆ (K (String × Prompt) ∩ ｛ cmd ｝)
done = generic doneP

pattern fetchP = inj₂ (inj₂ (inj₂ (inj₂ (inj₁ refl))))

fetch : cmd ∈ Hårss ⋆ (K Feed ∩ ｛ cmd ｝)
fetch = generic fetchP

pattern searchNextP = inj₂ (inj₂ (inj₂ (inj₂ (inj₂ refl))))

searchNext : input search ∈ Hårss ⋆ (K String ∩ ｛ input search ｝)
searchNext = generic searchNextP


prog : cmd ∈ Hårss ⋆ (K (String × Prompt) ∩ ｛ cmd ｝)
prog =
  move down >>
  prompt search >>
  putChar 'a' >>
  putChar 'p' >>
  putChar 'a' >>
  done
  where
  open RawPMonad rawPMonad
