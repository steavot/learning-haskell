-- From my very vague recollection of how basic 4 vectors work in flat
-- space-time.
-- 1st element is in the time direction, then the next 3 are the usual 3D
-- space.  When we dot product them a cheeky (-1) finds it's way into the
-- time dimension.
module SpaceTimeVectors
( V4(..)
, plusV4
, dotV4
, lenV4) where

data V4 t = V4 t t t t deriving(Show)

plusV4 :: (Num t) => V4 t -> V4 t -> V4 t
(V4 t x y z) `plusV4` (V4 t' x' y' z') = V4 (t+t') (x+x') (y+y') (z+z')

dotV4 :: (Num t) => V4 t -> V4 t -> t
(V4 t x y z) `dotV4` (V4 t' x' y' z') = (-1)*t*t' + x*x' + y*y' + z*z'

lenV4 :: (Floating t) => V4 t -> t
lenV4 v = sqrt . dotV4 v $ v
