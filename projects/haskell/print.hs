import Text.Printf

pi' = 3.14159

number m = read m :: Double

volume r = 4.0/3.0 * pi' * r ** 3

main :: IO ()
main = do
     r <- getline
     putStrLn ("VOLUME: " ++ printf "%.3f" (volume (numero r)))
