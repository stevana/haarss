{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Model.Window
  ( Window
  , Window'

  , above
  , prev
  , focus
  , next
  , below

  , makeWindow

  , closeWindow
  , closeWindow'

  , size

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

  , findFirst
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

------------------------------------------------------------------------
-- * Introduction

-- | Create a window of a certain size.
makeWindow :: Int -> [a] -> Window a
makeWindow _  []       = error "makeWindow []"
makeWindow sz (x : xs) =
  Window empty empty x (fromList ns) (fromList bs)
  where
  (ns, bs) = Prelude.splitAt (sz - 1) xs

------------------------------------------------------------------------
-- * Elimination

-- | Also see @Bifoldable@ and @Bitraversable@.

closeWindow :: Window a -> [a]
closeWindow (Window as ps x ns bs) = toList (as <> ps <> (x <| ns) <> bs)

closeWindow' :: (b -> a) -> Window' a b -> [a]
closeWindow' g = closeWindow . fmap g

------------------------------------------------------------------------
-- * Auxiliary functions

-- | The size of visible part of the window.
size :: Window' a b -> Int
size w = w^.prev.to length + 1 + w^.next.to length

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
-- * Search

findFirst :: Eq a => (a -> Bool) -> Window a -> Window a
findFirst p w | down w == w = w
              | otherwise   = go (down w)
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
