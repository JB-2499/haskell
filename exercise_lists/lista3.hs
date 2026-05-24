erronegativo = error "Digite um número positivo."

{-1. Como a versão recursiva da função fatorial se comporta se dermos a 
 - ela como argumento um número negativo? Modifique a implementação clássica 
 - para não permitir números negativos adicionando uma guarda ao passo recursivo.
-}

fatorial :: Int -> Int
fatorial 0 = 1
fatorial x | x < 0 = erronegativo
           | otherwise = x * fatorial (x - 1)

{-2. Defina a função recursiva somar :: Int -> Int que retorna a soma dos inteiros 
 - não-negativos a partir de um valor até zero. Por exemplo, somar 3 deve retornar 
 - 3+2+1+0 = 6.-}

somar :: Int -> Int
somar 0 = 0
somar x | x < 0 = erronegativo
        | otherwise = x + somar (x - 1)

{-3. Defina o operador de exponenciação ^ utilizando uma função recursiva, semelhante 
 - ao padrão usado para implementar a multiplicação com o operador *:
(*) :: Num a => a -> a -> a
m * 0 = 0
m * n = m + (m * (n - 1))
-}

pow :: (Num a, Integral b) => a -> b -> a
pow _ 0 = 1
pow m  n | n < 0 = erronegativo
        | otherwise = m * (m ^ (n - 1))

{-4. Defina a função euclides :: Int -> Int -> Int que implementa o algoritmo de Euclides 
 - para calcular o máximo divisor comum de dois inteiros não-negativos: se dois números 
 - são iguais, este número é o resultado; caso contrário, o menor número é subtraído do 
 - maior e o processo é repetido passando este novo número e o menor valor passado 
 - anteriormente como argumento. Exemplo:
> euclides 6 27
3
-}

euclides :: Int -> Int -> Int
euclides x y | x == y = x
             | x < 0 || y < 0 = erronegativo
             | otherwise = euclides subtr menor
                   where
                       maior = maximum [x,y]
                       menor = minimum [x,y]
                       subtr = maior - menor

{-5. Defina as funções abaixo usando recursão:
a) Decidir se todos os valores em uma lista são True:
	and :: [Bool] -> Bool
b) Concatenar uma lista de listas:
	concat :: [[a]] -> [a]
c) Produzir uma lista com n elementos idênticos:
	replicate :: Int -> a -> [a]
d) Selecionar o n-ésimo elemento em uma lista:
	(!!) :: [a] -> Int -> a
e) Decidir se um valor está presente em uma lista:
	elem :: Eq a => a -> [a] -> Bool
-}

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == True = and' xs
            | otherwise = False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++  concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y = y : replicate (x - 1) y

selec :: [a] -> Int -> a
selec [] _ = error "Forneça uma lista não vazia."
selec (x:xs) y | y < 0 = erronegativo
               | y == 0 = x
               | otherwise = selec xs (y-1)

elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys



{-6. Definir a função recursiva merge :: Ord a => [a] -> [a] -> [a] que une duas listas 
 - ordenadas em uma lista ordenada. Exemplo:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
-}

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys) | x < y = x : merge' xs (y:ys)
                     | otherwise = y : merge' ys (x:xs)

{-7. Usando a função merge, defina a função mergesort :: Ord a => [a] -> [a] que implementa o 
 - algoritmo de ordenação merge sort, que por sua vez considera uma lista vazia e uma lista com 
 - apenas um elemento como listas ordenadas, e que qualquer outra lista é ordenada a partir da 
 - união de duas listas que resultaram da ordenação das suas metades separadamente. Dica: primeiro 
 - implemente a função metades :: [a] -> ([a],[a]) que separa uma lista em duas partes cujos 
 - comprimentos são iguais ou no máximo diferem em uma unidade.-}

metades :: [a] -> ([a],[a])
metades [] = ([],[])
metades lista = (take meio lista, drop meio lista)
        where
            meio = div (length lista) 2

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort lista = merge' (mergesort esq) (mergesort dir)
        where
            (esq, dir) = metades lista

{-8. Implemente recursivamente funções que:
a) calcule a soma de uma lista de inteiros;
b) obtenha o número de elementos de uma lista;
c) selecione o último elemento de uma lista não-vazia.-}

summit :: [Int] -> Int
summit [] = 0
summit (x:xs) = x + summit xs

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

ultimo :: [a] -> a
ultimo [] = error "Forneça uma lista não vazia."
ultimo [x] = x
ultimo (x:xs) = ultimo xs
