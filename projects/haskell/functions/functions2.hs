tamanho :: [a] -> Int
tamanho xs | xs == [a] = sum [1 | _ <- xs]
           | xs == [] = 0
           | otherwise = error "Error! Type not suported by this function."
