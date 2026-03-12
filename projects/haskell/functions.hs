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
