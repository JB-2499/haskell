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

{-3. Defina uma função que repita as ocorrências até um determinado valor, no 
 - formato de uma lista, tal que (NÃO PODE USAR O replicate):
Exemplo: repete 4 
Saída 1: [ [4,4,4,4], [3,3,3], [2,2], [1] ]
Saída 2: [4,4,4,4,3,3,3,2,2,1]
-}

replicar :: Int -> [Int]
replicar 0 = []
replicar x = [x | y <- [1 .. x]]

repetir :: Int -> [Int]
repetir 0 = []
repetir x = replicar x ++ repetir (x-1)

repetir' :: Int -> [[Int]]
repetir' 0 = []
repetir' x = replicar x :  repetir' (x-1)

{-4. Construa uma função que cheque se o conteúdo de uma lista é um palíndromo:
palindromo [1,2,3,4,5] = False
palindromo [1,2,3,2,1] = True
palindromo [1,2,2,1] = True
-}

reversa :: [a] -> [a]
reversa [] = []
reversa [x] = [x]
reversa xs = xs !! (length xs - 1) : reversa (take (length xs - 1) xs)

palindromo :: [Int] -> Bool
palindromo xs = reversa xs == xs
