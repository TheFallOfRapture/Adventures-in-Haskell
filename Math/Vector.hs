module Math.Vector
    (
    Vector(..)
    ) where

data Vector a = Vector (a, a, a) deriving (Eq, Show)

instance Functor Vector where
    fmap f (Vector (x, y, z)) = Vector (f x, f y, f z)

instance Applicative Vector where
    pure f = Vector (f, f, f)
    (Vector (f, g, h)) <*> (Vector (x, y, z)) = (Vector (f x, g y, h z))

instance (Num a) => Num (Vector a) where
    Vector (x1, y1, z1) + Vector (x2, y2, z2) = Vector (x1 + x2, y1 + y2, z1 + z2)
    Vector (x1, y1, z1) - Vector (x2, y2, z2) = Vector (x1 - x2, y1 - y2, z1 - z2)
    Vector (x1, y1, z1) * Vector (x2, y2, z2) = Vector (x1 * x2, y1 * y2, z1 * z2)
    abs v = fmap abs v
    signum v = fmap signum v
    fromInteger x = pure $ fromInteger x
