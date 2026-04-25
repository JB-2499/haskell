import Data.List (group)
import Data.Char (isDigit)

{-1. Fornecidos três valores a, b e c, escreva uma função
 que retorne quantos dos três são iguais. A resposta pode 
 ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) 
 ou 0 (todos diferentes).-}

comparar :: Eq a => a -> a -> a -> Int
comparar x y z | (x == y) && (y == z) = 3
               | (x == y) && (y /= z) = 2
               | otherwise = 0

{-2. Fornecidos três valores a, b e c, elaborar uma função que 
retorne quantos desses três valores são maiores que a média entre 
eles.-}

media :: Fractional a => [a] -> a
media xs = sum xs / fromIntegral (length xs)

maiorMedia :: (Ord a, Fractional a) => [a] -> Int
maiorMedia xs = length (filter (> media xs) xs)

{-3. Escreva uma função potencia_2 que retorne o quadrado de um número.-}

potencia_2 :: Num a => a -> a
potencia_2 x = x*x

{-4. Reutilizando a função potencia_2, construir uma função potencia_4 
que retorne o seu argumento elevado à quarta potência.-}

potencia_4 :: Num a => a -> a
potencia_4 x = potencia_2 x * potencia_2 x

{-5. Implemente em Haskell a função do ou-exclusivo.-}

exor :: Eq a => a -> a -> Bool
exor x y | x == y = False
         | x /= y = True

{-6. Escrever duas funções, x_maior que retorne o maior e x_menor que 
retorne o menor valor real, das raízes de uma equação do segundo grau.-}

equacaoSeGrau :: (Fractional a, Floating a) => a -> a -> a -> [a]
equacaoSeGrau a b c = [((-b) + sqrt (b*b -4 * a * c)) / (2*a), ((-b) - sqrt (b*b -4 * a * c)) / (2*a)]

x_maior :: (Floating a, Ord a) => a -> a -> a -> a
x_maior a b c = maximum (equacaoSeGrau a b c)

x_menor :: (Floating a, Ord a) => a -> a -> a -> a
x_menor a b c = minimum (equacaoSeGrau a b c)

{-7. Criar funções que calculam a soma dos números entre n1 e n2, incluindo 
 - e excluindo os limites.-}

somar_ni :: (Enum a, Num a) => a -> a -> a
somar_ni x y = sum [x .. y]

somar_ne :: (Enum a, Num a) => a -> a -> a
somar_ne x y = sum [(x+1) .. (y-1)]

{-8. Dados três números n1, n2 e n3, encontrar os múltiplos de n3 que se 
 - encontram no  intervalo [n1,n2] (inclusivo).-}

verify_mult :: Int -> Int -> Bool
verify_mult x y = (x `mod` y) == 0

multiplos :: Int -> Int -> Int -> [Int]
multiplos x y z = [a | a <- [x .. y], verify_mult a z]

{-9. Utilizando a função sum, faça uma função que calcule a multiplicação 
 - entre dois números quaisquer, considerando números positivos e 
 - negativos. Não é permitido usar a operação de multiplicação.-}

altMult :: Int -> Int -> Int 
altMult x y | y >= 0 = sum [x | _ <- [1 .. y]]
            | otherwise = sum [(-x) | _ <- [1 .. (abs y)]]

{-10. Implemente a função mod2, que retorna o resto de uma divisão de 
 - inteiros. OBS: não é permitido usar a função mod nem a função rem 
 - da biblioteca.-}

mod2 :: Int -> Int -> Int
mod2 x y = x - ((div x y)^2)

{-11.-}

sequenciaSqrt :: Int -> Double
sequenciaSqrt 1 = sqrt 6
sequenciaSqrt n | n <= 0 = 0
                | otherwise = sqrt (6 + sequenciaSqrt (n - 1))

{-12. Implementar a fórmula que indica de quantas maneiras é 
 - possível escolher n objetos de uma coleção original de m 
 - objetos, onde m >= n.-}

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * (fatorial (n - 1))

colecao :: [a] -> Int -> Int
colecao _ 0 = 1
colecao [] _ = 0
colecao m n | (length m) < n = error "Erro, m deve ser maior que n!"
            | otherwise = div (fatorial m') (fatorial n * fatorial (m' - n))
            where m' = length m

{-13. Defina uma função que, dada uma lista numérica, retorne 
 - uma tupla, que contenha o maior da lista bem como índice 
 - na lista.-}

maiorTp :: [Int] -> (Int, Int)
maiorTp [] = error "A lista não pode estar vazia!"
maiorTp (x:xs) = aux xs 1 x 0
    where
        aux [] _ maiorValor idxMaiorValor = (maiorValor,idxMaiorValor)
        aux (y:ys) i atualMaior idxAtualMaior
            | y > atualMaior = aux ys (i + 1) y i
            | otherwise = aux ys (i+1) atualMaior idxAtualMaior

{-14. Defina uma função que converta uma lista de dígitos 
 - (unitários, 0 a 9) em uma outra lista, que é a sua tradução 
 - em String-}

numerais = [0,1,2,3,4,5,6,7,8,9]

numeraisStr = ["zero","um","dois","tres","quatro","cinco","seis","sete","oito","nove"]

dicionario :: [(Int,String)]
dicionario = zip numerais numeraisStr

tradNumStr :: [Int] -> [String]
tradNumStr [] = []
tradNumStr (x:xs) | x < 0 || x > 9 = error "Erro! Só existem 10 numerais (0 a 9)."
                  | otherwise = snd (dicionario !! x) : tradNumStr xs

{-15. Construa uma função del_posicao_n :: [Int] -> Int -> [Int] 
 - em que dada uma lista de inteiros e a posição de um 
 - elemento qualquer, retorne uma nova lista sem aquele 
 - elemento da n-ésima posição.-}

delPosicao :: [Int] -> Int -> [Int]
delPosicao [] _ = []
delPosicao (x:xs) 0 = xs
delPosicao (x:xs) i = x : delPosicao xs (i - 1)

{-16. Construa uma função inserir_posicao_x :: [Int] -> Int 
 - -> Int -> [Int] em que, dada uma lista de inteiros, 
 - uma posição e um elemento a ser inserido, retorne uma 
 - nova lista com aquele elemento na nésima posição.-}

inserir_posicao :: [Int] -> Int -> Int -> [Int]
inserir_posicao [] _ num  = [num]
inserir_posicao lista 0 num = num : lista
inserir_posicao (x:xs) idx num = x : inserir_posicao xs (idx - 1) num

{-17. Defina uma função que retorne o valor da n-ésima 
 - posição de uma lista.-}

retN :: [a] -> Int -> a
retN [] _ = error "Erro! A lista não pode estar vazia."
retN (x:xs) n | n < 0 = error "Erro! Indices negativos não são permitidos."
              | n == 0 = x
              | otherwise = retN xs (n - 1)

{-18. Dadas duas listas ordenadas como entrada, faça 
 - uma função merge, que une as duas listas.-}

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)

inserir :: Ord a => a -> [a] -> [a]
inserir x [] = [x]
inserir x (y:ys) | x <= y = x : y : ys
                 | otherwise = y : inserir x ys

mergeList :: Ord a => [a] -> [a] -> [a]
mergeList [] y = y ++ []
mergeList x [] = x ++ []
mergeList x y = ordenar (x ++ y)

{-19. Implemente uma função que receba duas listas 
 - de inteiros sem repetição, e retorne uma terceira 
 - lista que contenha somente números que estejam 
 - nas duas listas dadas.-}

repeticao :: [Int] -> [Int] -> [Int]
repeticao [] _ = []
repeticao (x:xs) l | x `elem` l = x : repeticao xs l
                   | otherwise = repeticao xs l

{-20. Crie uma função que faça uma compressão sobre 
 - uma sequência de caracteres iguais, substitua a 
 - sequência por !na, onde n é o número de vezes que 
 - o caractere a é repetido. Observe que só devem ser 
 - comprimidas sequências de tamanhos maiores que 3.-}

comprimir :: String -> String
comprimir s = concatMap formatar (group s)
    where
        formatar :: String -> String
        formatar grupo | length grupo > 3 = "!" ++ show (length grupo) ++ [head grupo]
                       | otherwise = grupo

{-21. Implemente uma função que descomprima o texto 
 - resultante da função anterior.-}

descomp :: String -> String
descomp [] = []
descomp ('!':xs) = 
    let (numStr, c:resto) = span isDigit xs
        n = read numStr :: Int
    in replicate n c ++ descomp resto
descomp (x:xs) = x : descomp xs
