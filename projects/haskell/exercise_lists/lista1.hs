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
