{-1. Usando compreensão de listas, forneça uma expressão que calcula a soma 
 - 1² +2²+...+100² dos quadrados dos primeiros 100 números inteiros.-}

somaQuad = [x*x | x <- [1 .. 100]]

{-2. Suponha que um plano de coordenadas de tamanho m x n é dado pela lista 
 - de todos os pares (x,y) de inteiros tal que 0  x  m e 0 y n. Usando compreensão 
 - de listas, defina a função grid :: Int -> Int -> [(Int,Int)] que retorna o plano 
 - de coordenadas de um dado tamanho. Por exemplo:
 - > grid 1 2
 - [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)].-}

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(m,n) | m <- [0 .. x], n <- [0 .. y]]

{-3. Usando compreensão de listas e a função grid definida na questão anterior, 
 - defina uma função quadrado :: Int -> [(Int,Int)] que retorna um plano de 
 - coordenadas quadrado de tamanho n, excluindo a diagonal principal (0,0) a (n,n). 
 - Por exemplo:
 - > quadrado 2
 - [(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)].-}

quadrado :: Int -> [(Int,Int)]
quadrado n = [(x,y) | (x,y) <- grid n n, x /= y]

{-4. De maneira similar à função length, mostre como a função replicate :: Int -> a -> [a] 
 - que produz uma lista de elementos idênticos pode ser definida usando compreensão de 
 - listas. Exemplo:
 - > replicate 3 True
 - [True, True, True].-}


replicar :: Int -> a -> [a]
replicar x y = [y | a <- [0 .. x]]

{-5. Uma tupla (x,y,z) de inteiros positivos é Pitagoreana se satisfaz a equação x² + y² = z². 
 - Usando compreensão de listas com três geradores, defina a função pitag :: Int -> [(Int, Int, Int)] 
 - que retorna uma lista de todas as tuplas que satisfazem a condição estabelecida e cujos 
 - componentes são menores ou iguais a um dado limite. 
 - Exemplo: 
 - > pitag 10
 - [(3,4,5), (4,3,5), (6,8,10), (8,6,10)]-}

pitag :: Int -> [(Int,Int,Int)]
pitag n = [(x,y,z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]

{-6. Um inteiro positivo é perfeito se ele é igual à soma de todos os seus fatores, 
 - excluindo o próprio número. Usando compreensão de listas e a função fatores, defina a 
 - função perfeitos :: Int -> [Int] que retorna a lista de todos os números perfeitos 
 - menores que um limite informado como argumento. 
 - Exemplo:
 - > perfeitos 500
 - [6, 28, 496]-}

fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. (n - 1)], (mod n x) == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1 .. (n - 1)], sum (fatores x) == x]

{-7. Mostre que a compreensão de lista [(x,y) | x <- [1,	2], y <- [3,4]], 
 - com dois geradores, pode ser representada usando duas compreensões de lista, 
 - cada uma com apenas um gerador. Dica: Procure usar a função concat.-}

suc :: (Eq a, Num a) => a -> a -> a
suc x 0 = x
suc x y = x + (suc 1 (y - 1))
