{-1. Implemente uma função que receba uma lista de inteiros (que pode ou não estar ordenada) 
e retorne uma lista ordenada em ordem crescente formada apenas pelos números ímpares da lista 
recebida.
Exemplo: impares [3,6,4,8,1,9,7]
Saída: [1,3,7,9]
-}

-- Auxiliar
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = ordenar menores ++ [x] ++ ordenar maiores
                 where
                     menores = [y | y <- xs, y <= x]
                     maiores = [y | y <- xs, y > x]
-- Recursiva
impares :: [Int] -> [Int]
impares [] = []
impares (x:xs) | odd x = ordenar (x : impares xs)
               | otherwise = ordenar (impares xs)

-- Compreensiva
impares' :: [Int] -> [Int]
impares' [] = []
impares' xs = ordenar [x | x <- xs, odd x]

{-2. Defina uma função que retorne o elemento na n-ésima posição de uma lista.
Exemplo: posicao 2 [‘a’, ‘b’, ‘c’, ‘d’
Saída: ‘c’
-}

-- Recursiva
posicao :: Eq a => Int -> [a] -> a
posicao _ [] = error "Lista vazia."
posicao x (y:ys) | x == 0 = y
                 | otherwise = posicao (x-1) ys

-- Compreensiva
posicao' :: Eq a => Int -> [a] -> a
posicao' _ [] = error "Lista vazia."
posicao' k xs = head [v | (k',v) <- zip [0..] xs, k == k']

{-3. Defina uma função que repita as ocorrências até um determinado valor, no 
 - formato de uma lista, tal que (NÃO PODE USAR O replicate):
Exemplo: repete 4 
Saída 1: [ [4,4,4,4], [3,3,3], [2,2], [1] ]
Saída 2: [4,4,4,4,3,3,3,2,2,1]
-}

-- Auxiliar
replicar :: Int -> [Int]
replicar 0 = []
replicar x = [x | y <- [1 .. x]]

-- Recursiva
repetir :: Int -> [Int]
repetir 0 = []
repetir x = replicar x ++ repetir (x-1)

-- Compreensiva
repetir' :: Int -> [[Int]]
repetir' 0 = []
repetir' x = replicar x :  repetir' (x-1)

{-4. Construa uma função que cheque se o conteúdo de uma lista é um palíndromo:
palindromo [1,2,3,4,5] = False
palindromo [1,2,3,2,1] = True
palindromo [1,2,2,1] = True
-}

-- Auxiliar
reversa :: [a] -> [a]
reversa [] = []
reversa [x] = [x]
reversa xs = xs !! (length xs - 1) : reversa (take (length xs - 1) xs)

-- Principal
palindromo :: [Int] -> Bool
palindromo xs = reversa xs == xs

{-5. Construa uma função que retorne os n primeiros elementos da sequência de Fibonacci:
Exemplo: fibonacci 10
Saída: [0,1,1,2,3,5,8,13,21,34]
-}

-- Auxiliar I
sequencia :: Int -> Int -> Int -> [Int]
sequencia _ _ 0 = []
sequencia x y stop = x : sequencia y (x + y) (stop -1)

-- Principal
fibonacci :: Int -> [Int]
fibonacci n = sequencia 0 1 n

-- Auxiliar II
fib :: [Int]
fib = 0 : 1 : [a + b | (a,b) <- zip fib (tail fib)]

-- Alternativa
fibonacci' :: Int -> [Int]
fibonacci' n = take n fib 

{-6. Sem olhar as definições no Prelude, defina a seguintes funções de alta ordem:
Decide se todos os elementos de uma lista satisfazem um predicado:
all :: (a -> Bool) -> [a] -> Bool
Decide se algum elemento de uma lista satisfaz um predicado:
any :: (a -> Bool) -> [a] -> Bool
Selecione elementos de uma lista enquanto eles satisfazem um predicado:
takeWhile :: (a -> Bool) -> [a] -> [a]
Remove elementos de uma lista enquanto eles satisfazem um predicado:
dropWhile :: (a -> Bool) -> [a] -> [a]
-}

-- a) Recursiva
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos func (x:xs) | func x = todos func xs
                  | otherwise = False
-- a) Compreensiva
todos' :: Eq a => (a -> Bool) -> [a] -> Bool
todos' func xs = xs == [x | x <- xs, func x == True]

-- b) Recursiva
algum :: (a -> Bool) -> [a] -> Bool
algum _ [] = False
algum func (x:xs) | func x = True
                  | otherwise = algum func xs

-- b) Compreensiva
algum' :: Eq a => (a -> Bool) -> [a] -> Bool
algum' func xs = length [x | x <- xs, func x == True] > 0

-- c) Principal
pegawhile :: (a -> Bool) -> [a] -> [a]
pegawhile _ [] = []
pegawhile func (x:xs) | func x = x : pegawhile func xs
                      | otherwise = []

-- d) Principal
tirawhile :: (a -> Bool) -> [a] -> [a]
tirawhile _ [] = []
tirawhile func (x:xs) | func x = tirawhile func xs
                      | otherwise = x:xs
