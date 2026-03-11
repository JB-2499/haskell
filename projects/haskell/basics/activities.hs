main :: IO ()
main = do
    print $ last' [1, 2, 3, 4]
    print $ somar 1 2

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs

somar :: Int -> Int -> Int
somar x y = x + y
