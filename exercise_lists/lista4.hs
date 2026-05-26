{-1. Implemente uma função que receba uma lista de inteiros (que pode ou não estar ordenada) 
e retorne uma lista ordenada em ordem crescente formada apenas pelos números ímpares da lista 
recebida.
Exemplo: impares [3,6,4,8,1,9,7]
Saída: [1,3,7,9]
-}

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = ordenar menores ++ [x] ++ ordenar maiores
                 where
                     menores = [y | y <- xs, y <= x]
                     maiores = [y | y <- xs, y > x]

impares :: [Int] -> [Int]
impares [] = []
impares (x:xs) | odd x = ordenar (x : impares xs)
               | otherwise = ordenar (impares xs)

impares' :: [Int] -> [Int]
impares' [] = []
impares' xs = ordenar [x | x <- xs, odd x]

{-2. Defina uma função que retorne o elemento na n-ésima posição de uma lista.
Exemplo: posicao 2 [‘a’, ‘b’, ‘c’, ‘d’
Saída: ‘c’
-}

posicao :: Eq a => Int -> [a] -> a
posicao _ [] = error "Lista vazia."
posicao x (y:ys) | x == 0 = y
                 | otherwise = posicao (x-1) ys

posicao' :: Eq a => Int -> [a] -> a
posicao' _ [] = error "Lista vazia."
posicao' k xs = head [v | (k',v) <- zip [0..] xs, k == k']
