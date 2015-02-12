module MonadProblem where

-- Based on http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

import Data.Complex

-----------------------------------
f, g :: Float -> Float

f v = 2.0 * v
g v = v * v

makeDebugable :: (Float -> Float) -> String -> (Float -> (Float, String))
makeDebugable f s v = let res = f v in (res, s ++ show v ++ " -> " ++ show res)

f', g', unit :: Float -> (Float, String)
f' v = makeDebugable f "mul by 2 " v
g' v = makeDebugable g "squaring " v

unit v = (v, "")
 
bind :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind f' (gv, gs) = let (fv, fs) = f' gv in (fv, gs ++ " THEN " ++ fs)

-----------------------------------------------------------------------
-- k E [0, n)
nthComplexRoot :: (RealFloat a) => Complex a -> Int -> Int -> Complex a
nthComplexRoot c n k = (rsq * cos drb) :+ (rsq * sin drb)
			where 
				(r, x) = polar c
				drb = (x + (fromIntegral k) * 2.0 * pi) / fromIntegral n
				rsq = r ** (1.0 / fromIntegral n)

complexRoots :: (RealFloat a) => Complex a -> Int -> [Complex a]
complexRoots c n = [nthComplexRoot c n k | k <- [0..n - 1]]

sqrt', cbrt' :: (RealFloat a) => Complex a -> [Complex a]
sqrt' c = complexRoots c 2
cbrt' c = complexRoots c 3


runTest :: (Show testResult) => String -> testResult -> String
runTest descr testResult = "test '" ++ descr ++ "':\t" ++ (show testResult) ++ "\n"

test :: String
test = "Testing begins\n"
		++ runTest "g, f conveyer" (bind f' $ g' 3)
		++ runTest "complex num" (cbrt' (1 :+ 0))