
-- This prototype tries to map Sodium concepts to the Reactive banana
-- library.

import Reactive.Banana
import Reactive.Banana.Frameworks

------------------------------------------------------------------------

-- newEvent :: Reactive (Event a, a -> Reactive ())
-- newEvent :: Frameworks t => Moment t (Event t a, a -> IO ())

-- hold :: a -> Event a -> Reactive (Behavior a)
hold :: a -> Event t a -> Behavior t a
hold = stepper

-- updates :: Behavior a -> Event a
updates :: Frameworks t => Behavior t a -> Moment t (Event t (Future a))
updates = changes

-- accum  :: a -> Event (a -> a) -> Reactive (Behavior a)
accum :: a -> Event t (a -> a) -> Event t a
accum = accumE

-- filterJust :: Event   (Maybe a) -> Event a
-- filterJust :: Event t (Maybe a) -> Event t a

-- executeAsyncIO :: Event Plain (IO a) -> Event Plain a
-- execute :: Frameworks t
--         => Event t (FrameworksMoment a) -> Moment t (Event t a)
-- liftIO :: Frameworks t => IO a -> Moment t a

executeAsyncIO :: Frameworks t => Event t (IO a) -> Moment t (Event t a)
executeAsyncIO = execute . fmap (\io -> FrameworksMoment (liftIO io))

-- collectE :: (a -> s -> (b, s)) -> s -> Event a -> Reactive (Event b)
-- mapAccum :: acc -> Event t (acc -> (x, acc)) ->
--             (Event t x, Behavior t acc)

collectE :: Frameworks t
         => (a -> s -> (b, s)) -> s -> Event t a -> Event t b
collectE f s = fst . mapAccum s . fmap f

-- snapshot :: (a -> b -> c) -> Event a -> Behavior b -> Event c
snapshot :: Frameworks t
         => (a -> b -> c) -> Event t a -> Behavior t b -> Event t c
snapshot f ea bb = fmap (flip f) bb <@> ea

-- listen :: Event a -> (a -> IO ()) -> Reactive (IO ())
-- reactimate :: Frameworks t => Event t (IO ()) -> Moment t ()
listen :: Frameworks t => Event t a -> (a -> IO ()) -> Moment t ()
listen ea f = reactimate $ fmap f ea

-- value :: Behavior a -> Event a
-- initial :: Behavior t a -> Moment t a
-- changes :: Frameworks t => Behavior t a -> Moment t (Event t (Future a))
listenValue :: Frameworks t
            => Behavior t a -> (a -> IO ()) -> Moment t ()
listenValue ba f = do
  a <- initial ba
  liftIO $ f a
  efa <- updates ba
  reactimate' $ fmap (fmap f) efa
