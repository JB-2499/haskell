main :: IO ()
main = do
     putStrLn "Hello, World!"
     putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [1 .. 10]))
     putStrLn "\n\nNow, look at this very cool function, \"head\"."
     putStrLn "\nIt returns the first element from a list."
     putStrLn "\n\nFor example, if you take the following list: [1, 2, 3, 4 ,5]"
     putStrLn ("It returns: " ++ show (head [1, 2, 3, 4, 5]) ++ " as a result.")
