import Text.Printf

somar :: Int -> Int -> Int
somar x y = x + y

power :: Num a => a -> a
power x = x * x

calcPct :: Integral a => a -> a -> Double
calcPct x y = (fromIntegral x / fromIntegral y) * 100

pctShow :: Integral a => a -> a -> IO ()
pctShow x y = printf "Result: %.1f%%\n" (calcPct x y)

somarL :: Num a => [a] -> a
somarL [] = 0
somarL (x:xs) = x + somarL xs

juntar :: String -> String -> String
juntar sx sy = sx ++ sy

listarNum :: (Num a, Show a) => [a] -> String
listarNum [] = ""
listarNum [x] = show x
listarNum (x:xs) = show x ++ ", " ++ listarNum xs

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNum x = if x > 100 then x else x*2

f x = 2*x + 1
