module Control.Arrow.Extras where

import Control.Arrow

-- | 'arr' under 'Kleisli'
arrK :: Monad m => (b -> c) -> (b -> m c)
arrK f = runK (arr f)

-- | 'first' under 'Kleisli'
firstK :: Monad m => (b -> m c) -> ((b, d) -> m (c, d))
firstK = underK1 first

-- | 'second' under 'Kleisli'
secondK :: Monad m => (b -> m c) -> ((d, b) -> m (d, c))
secondK = underK1 second

-- | '***' under 'Kleisli'
-- The @|<@ looks like a @K@
(***|<) :: Monad m => (b -> m c) -> (b' -> m c') -> ((b, b') -> m (c, c'))
(***|<) = underK2 (***)
infixr 3 ***|< -- like ***

-- | '&&&' under 'Kleisli'
-- The @|<@ looks like a @K@
(&&&|<) :: Monad m => (b -> m c) -> (b -> m c') -> (b -> m (c, c'))
(&&&|<) = underK2 (&&&)
infixr 3 &&&|< -- like (&&&)

-- | 'left' under 'Kleisli'
leftK :: Monad m => (b -> m c) -> (Either b d -> m (Either c d))
leftK = underK1 left

-- | 'right' under 'Kleisli'
rightK :: Monad m => (b -> m c) -> (Either d b -> m (Either d c))
rightK = underK1 right

-- | '+++' under 'Kleisli'
-- The @|<@ looks like a @K@
(+++|<) :: Monad m =>  (b -> m c) -> (b' -> m c')
    -> (Either b b' -> m (Either c c'))
(+++|<) = underK2 (+++)
infixr 2 +++|< -- like (+++)

-- | '+++' under 'Kleisli'
-- The @|<@ looks like a @K@
(||||<) :: Monad m => (b -> m d) -> (c -> m d) -> (Either b c -> m d)
(||||<) = underK2 (|||)
infixr 2 ||||< -- like (+++)

-- | shorter name for 'runKleisli'
runK :: Kleisli m a1 b1
    -> (a1 -> m b1)
runK = runKleisli

-- | run under 'Kleisli'
underK1 :: (Kleisli m a1 b1 -> Kleisli m a2 b2)
    -> (a1 -> m b1) -> (a2 -> m b2)
underK1 f x = runKleisli $ f (Kleisli x)

-- | run binary function under 'Kleisli'
underK2 :: (Kleisli m a1 b1 -> Kleisli m a2 b2 -> Kleisli m a3 b3)
    -> (a1 -> m b1) -> (a2 -> m b2) -> (a3 -> m b3)
underK2 f x y = runKleisli $ f (Kleisli x) (Kleisli y)

