{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Haarss.Model.Window
  ( Window
  , Window'

  , above
  , prev
  , focus
  , next
  , below

  , makeWindow
  , window

  , closeWindow
  , closeWindow'

  , size
  , add
  , remove
  , replace

  , down
  , down'
  , up
  , up'
  , bot
  , bot'
  , top
  , top'

  , resize
  , resize'

  , rearrangeUp
  , rearrangeDown

  , findFirst

  , htf_thisModulesTests
  )
  where

import           Prelude             hiding (length, null)
import qualified Prelude

import           Control.Applicative hiding (empty)
import           Control.Lens        hiding (below, (<|), (|>))
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Foldable
import           Data.Monoid
import           Data.Sequence       as Seq
import           Test.Framework      hiding (resize)
import           Test.QuickCheck     hiding (resize)

------------------------------------------------------------------------
-- * Types

-- | Homogeneous windows.
type Window a = Window' a a

-- | Heterogeneous windows.
data Window' a b = Window
  { _above :: Seq a
  , _prev  :: Seq a
  , _focus :: b
  , _next  :: Seq a
  , _below :: Seq a
  }
  deriving (Eq, Functor)

makeLenses ''Window'

instance Bifunctor Window' where
  bimap f g (Window as          ps          y     ns          bs) =
             Window (fmap f as) (fmap f ps) (g y) (fmap f ns) (fmap f bs)

instance Bifoldable Window' where
  bifoldMap f g (Window as ps y ns bs) =
    foldMap f as <> foldMap f ps <> g y <> foldMap f ns <> foldMap f bs

instance Bitraversable Window' where
  bitraverse f g (Window as ps y ns bs) =
    Window <$> traverse f as <*> traverse f ps <*> g y
           <*> traverse f ns <*> traverse f bs

instance (Show a, Show b) => Show (Window' a b) where
  show (Window as ps x ns bs) = unlines
    [ show (toList as)
    , Prelude.replicate 10 '-'
    , show (toList ps)
    , "<<" ++ show x ++ ">>"
    , show (toList ns)
    , Prelude.replicate 10 '-'
    , show (toList bs)
    ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Window' a b) where
  arbitrary = do
    as <- fromList <$> arbitrary
    ps <- fromList <$> arbitrary
    y  <- arbitrary
    ns <- fromList <$> arbitrary
    bs <- fromList <$> arbitrary
    return $ Window as ps y ns bs

  shrink (Window as ps y ns bs) =
    [ Window as' ps  y  ns  bs  | as' <- shrinkSeq shrink as ] ++
    [ Window as  ps' y  ns  bs  | ps' <- shrinkSeq shrink ps ] ++
    [ Window as  ps  y' ns  bs  | y'  <- shrink y ] ++
    [ Window as  ps  y  ns' bs  | ns' <- shrinkSeq shrink ns ] ++
    [ Window as  ps  y  ns  bs' | bs' <- shrinkSeq shrink bs ]
    where
    shrinkSeq :: (a -> [a]) -> Seq a -> [Seq a]
    shrinkSeq f = map fromList . shrinkList f . toList

------------------------------------------------------------------------
-- * Introduction

-- | Create a window of a certain size.
makeWindow :: Int -> [a] -> Window a
makeWindow _  []       = error "makeWindow []"
makeWindow sz (x : xs) =
  Window empty empty x (fromList ns) (fromList bs)
  where
  (ns, bs) = Prelude.splitAt (sz - 1) xs

-- | Smart constructor for creating a window via lists.
window :: [a] -> [a] -> a -> [a] -> [a] -> Window a
window as ps x ns bs =
  Window (fromList as) (fromList ps) x (fromList ns) (fromList bs)

------------------------------------------------------------------------
-- * Elimination

-- | Also see @Bifoldable@ and @Bitraversable@.

closeWindow :: Window a -> [a]
closeWindow (Window as ps x ns bs) = toList (as <> ps <> (x <| ns) <> bs)

closeWindow' :: (b -> a) -> Window' a b -> [a]
closeWindow' g = closeWindow . fmap g

------------------------------------------------------------------------
-- * Queries

-- | The size of visible part of the window.
size :: Window' a b -> Int
size w = w^.prev.to length + 1 + w^.next.to length

canMoveDown :: Window a -> Bool
canMoveDown w = w^.next.to length + w^.below.to length > 0

canMoveUp :: Window a -> Bool
canMoveUp w = w^.above.to length + w^.prev.to length > 0

------------------------------------------------------------------------
-- * Adding, removing and replacing elements

add :: a -> Window a -> Window a
add x w = resize (size w) $
  w & prev  %~ (|> w^.focus)
    & focus .~ x

remove :: Window a -> Window a
remove w@(Window _ _  _ (viewl -> n :< ns) _)
  = resize (size w) $ w & focus .~ n
                        & next  .~ ns
remove w@(Window _ _  _ (viewl -> EmptyL) (viewl -> b :< bs))
  = resize (size w) $ w & focus .~ b
                        & below .~ bs
remove w@(Window _ (viewr -> ps :> p) _ (viewl -> EmptyL) (viewl -> EmptyL))
  = resize (size w) $ w & focus .~ p
                        & prev  .~ ps
remove w@(Window (viewr -> as :> a) (viewr -> EmptyR) _
                 (viewl -> EmptyL)  (viewl -> EmptyL))
  = resize (size w) $ w & focus .~ a
                        & above .~ as
remove w@(Window (viewr -> EmptyR)  (viewr -> EmptyR) _
                 (viewl -> EmptyL)  (viewl -> EmptyL))
  = w
remove _
  = error "Impossible."

prop_addRemove :: Int -> Window Int -> Bool
prop_addRemove x w =
  closeWindow (remove (add x w)) == closeWindow w

replace :: Int -> a -> Window a -> Window a
replace pos x w@(Window as ps _ ns bs)
  | pos >= Prelude.length (closeWindow w)
                              = w
  | pos < las                 = w & above .~ update pos x as
  | pos - las < lps           = w & prev  .~ update (pos - las) x ps
  | pos - las - lps == 0      = w & focus .~ x
  | pos - las - lps - 1 < lns = w & next  .~ update
                                               (pos - las - lps - 1) x ns
  | otherwise                 = w & below .~ update
                                               (pos - las - lps - 1 - lns) x bs
  where
  las = Seq.length as
  lps = Seq.length ps
  lns = Seq.length ns

prop_replace :: Positive Int -> Int -> Window Int -> Bool
prop_replace (Positive pos) z w =
  closeWindow (replace pos z w) == replaceList pos z (closeWindow w)
  where
  replaceList :: Int -> a -> [a] -> [a]
  replaceList n x xs | n >= Prelude.length xs = xs
                     | otherwise              = xs1 ++ x : xs2
    where
    (xs1, (_ : xs2)) = Prelude.splitAt n xs

------------------------------------------------------------------------
-- * Movement

down :: Window a -> Window a
down w@(Window
  _
  {----------------}
  _
  _
  (viewl -> EmptyL)
  {----------------}
  (viewl -> EmptyL)) = w

down (Window
  as
  {----------------}
  (viewl -> EmptyL)
  x
  (viewl -> EmptyL)
  {----------------}
  (viewl -> b :< bs)) = Window (as |> x) empty b empty bs

down (Window
  as
  {----------------}
  (viewl -> p :< ps)
  x
  (viewl -> EmptyL)
  {----------------}
  (viewl -> b :< bs)) = Window (as |> p) (ps |> x) b empty bs

down (Window
  as
  {----------------}
  ps
  x
  (viewl -> n :< ns)
  {----------------}
  bs)                 = Window as (ps |> x) n ns bs
down _                = error "Impossible."

up :: Window a -> Window a
up w@(Window
  (viewr -> EmptyR)
  {---------------}
  (viewr -> EmptyR)
  _
  _
  {---------------}
  _)                  = w

up (Window
  as
  {----------------}
  (viewr -> ps :> p)
  x
  ns
  {----------------}
  bs)                 = Window as ps p (x <| ns) bs

up (Window
  (viewr -> as :> a)
  {----------------}
  (viewr -> EmptyR)
  x
  (viewr -> EmptyR)
  {----------------}
  bs)                 = Window as empty a empty (x <| bs)

up   (Window
  (viewr -> as :> a)
  {----------------}
  (viewr -> EmptyR)
  x
  (viewr -> ns :> n)
  {----------------}
  bs)                 = Window as empty a (x <| ns) (n <| bs)
up _                  = error "Impossible."

top, bot :: Window a -> Window a
top w = iterate up   w !! (w^.above.to length + w^.prev.to  length)
bot w = iterate down w !! (w^.next.to  length + w^.below.to length)

data Dir = Up | Down | Top | Bot
  deriving (Eq, Show)

instance Arbitrary Dir where
  arbitrary = Test.QuickCheck.elements [Up, Down, Top, Bot]

move :: [Dir] -> Window a -> Window a
move ds w = Prelude.foldl (\ih f -> f ih) w (map toMove ds)
  where
  toMove :: Dir -> Window a -> Window a
  toMove Up   = up
  toMove Down = down
  toMove Top  = top
  toMove Bot  = bot

prop_move :: Positive Int -> NonEmptyList Int -> [Dir] -> Bool
prop_move (Positive i) (NonEmpty xs) ds

  -- Movement preserves order and cardinality.
  =  closeWindow w' == closeWindow w

  -- Movement preserves the size of the window.
  && i >= size w'

  -- Moving to the top, or the bottom, shouldn't leave any items over,
  -- or under, the focus.
  && w'^.to top.above.to null && w'^.to top.prev.to null
  && w'^.to bot.next.to null && w'^.to bot.below.to null
  where
  w  = makeWindow i xs
  w' = move ds w

------------------------------------------------------------------------
-- * Resizing

padBot :: Int -> Window a -> Window a
padBot i w = w & next  %~ (<> bs)
               & below .~ bs'
    where
    (bs, bs') = Seq.splitAt i (w^.below)

padTop :: Int -> Window a -> Window a
padTop i w = w & above .~ as
               & prev  %~ (as' <>)
  where
  (as, as') = Seq.splitAt (w^.above.to length - i) (w^.above)

cropBot :: Int -> Window a -> Window a
cropBot i w = w & next  .~ ns
                & below %~ (ns' <>)
  where
  (ns, ns') = Seq.splitAt (w^.next.to length - i) (w^.next)

cropTop :: Int -> Window a -> Window a
cropTop i w = w & above %~ (<> ns)
                & prev  .~ ns'
  where
  (ns, ns') = Seq.splitAt i (w^.prev)

resize :: Int -> Window a -> Window a
resize i w = case compare i (size w) of
  EQ -> w

  LT -> cropTop (size w' - i) w'
    where
    w' = cropBot (size w - i) w

  GT -> padTop (i - size w') w'
    where
    w' = padBot (i - size w) w

prop_resize :: Positive Int -> Positive Int -> NonEmptyList Int -> [Dir] ->
               Bool
prop_resize (Positive i) (Positive j) (NonEmpty xs) ds

  -- Resizing with the current size doesn't do anything.
  =  resize i w == w

  -- Resizing preserves order and cardinality.
  && closeWindow (resize j w) == closeWindow w

  where
  w = move ds (makeWindow i xs)

prop_resizeSize :: Positive Int -> Positive Int -> NonEmptyList Int ->
                   [Dir] -> Property
prop_resizeSize (Positive i) (Positive j) (NonEmpty xs) ds =

  -- The new size will be the size we resize to, given that there are
  -- enough items to fill the window with.
  j <= Prelude.length xs ==>
  size (resize j w) == j
  where
  w = move ds (makeWindow i xs)

------------------------------------------------------------------------
-- * Rearrangement

flipDown :: Window a -> Window a
flipDown (Window as ps x (viewl -> n :< ns) bs)                 =
          Window as ps n (x <| ns)          bs
flipDown (Window as ps x (viewl -> EmptyL)  (viewl -> b :< bs)) =
          Window as ps b empty              (x <| bs)
flipDown w                                                      = w

flipUp :: Window a -> Window a
flipUp (Window as                 (viewr -> ps :> p) x ns bs) =
        Window as                 (ps |> x)          p ns bs
flipUp (Window (viewr -> as :> a) (viewr -> EmptyR)  x ns bs) =
        Window (as |> x)          empty              a ns bs
flipUp w                                                      = w

rearrangeDown :: Window a -> Window a
rearrangeDown w | canMoveDown w = flipUp (down w)
                | otherwise     = w

rearrangeUp :: Window a -> Window a
rearrangeUp w | canMoveUp w = flipDown (up w)
              | otherwise   = w

prop_rearrangeDown :: Window Int -> Property
prop_rearrangeDown w = w^.next.to length > 0 ==>
  rearrangeUp (rearrangeDown w) == w

prop_rearrangeUp :: Window Int -> Property
prop_rearrangeUp w = w^.prev.to length > 0 ==>
  rearrangeDown (rearrangeUp w) == w

------------------------------------------------------------------------
-- * Search

findFirst :: Eq a => (a -> Bool) -> Window a -> Window a
findFirst p w | canMoveDown w = go (down w)
              | otherwise     = w
  where
  go w' | w'^.focus.to p  = w'
        | down w' == w'   = w
        | otherwise       = go (down w')

------------------------------------------------------------------------
-- * Heteregenous versions of above functions

down', up', bot', top' :: (a -> b) -> (b -> a) -> Window' a b -> Window' a b
down' f g = fmap f . down . fmap g
up'   f g = fmap f . up   . fmap g
top'  f g = fmap f . top  . fmap g
bot'  f g = fmap f . bot  . fmap g

resize' :: (a -> b) -> (b -> a) -> Int -> Window' a b -> Window' a b
resize' f g i = fmap f . resize i . fmap g
