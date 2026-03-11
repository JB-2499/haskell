somar :: Int -> Int -> Int
somar x y = x + y

power :: Num a => a -> a
power x = x * x

pctInt :: Num a => a -> a -> a
pctInt x y = (fromIntegral x / fromIntegral y) * 100

pctFl :: a

