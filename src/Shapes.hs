module Shapes where

data Vector2 = Vector2 Float Float
data Shape = Circle Vector2 Float | Rectangle Vector2 Vector2

distance :: Vector2 -> Vector2 -> Float
distance a b = magnitude $ b `sub` a

sub :: Vector2 -> Vector2 -> Vector2
sub a b = applyBinary (-) a b

applyBinary :: (Float -> Float -> Float) -> Vector2 -> Vector2 -> Vector2
applyBinary f (Vector2 a b) (Vector2 c d) = Vector2 (f a c) (f b d)

applyUnary :: (Float -> Float) -> Vector2 -> Vector2
applyUnary f (Vector2 a b) = Vector2 (f a) (f b)

vol :: Vector2 -> Float
vol (Vector2 x y) = x * y

magnitude :: Vector2 -> Float
magnitude (Vector2 x y) = sqrt $ x ^ 2 + y ^ 2

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle pt pt2) = abs . vol $ pt2 `sub` pt  


test :: String
test = show . area $ Rectangle (Vector2 0.0 0.0) (Vector2 1.0 1.0)