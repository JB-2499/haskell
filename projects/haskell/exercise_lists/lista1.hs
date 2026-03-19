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

{-Escrever duas funções, x_maior que retorne o maior e x_menor que 
retorne o menor valor real, das raízes de uma equação do segundo grau.-}

equacaoSeGrau :: Floating a => a -> a -> a -> [a]
equacaoSeGrau a b c = [((-b) + sqrt (b*b -4 * a * c)) / (2*a), ((-b) - sqrt (b*b -4 * a * c)) / (2*a)]

x_maior :: (Floating a, Ord a) => a -> a -> a -> a
x_maior a b c = maximum (equacaoSeGrau a b c)
