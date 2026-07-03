{-1. Semelhante à função somar, defina uma função de multiplicação recursiva para números naturais mult :: Nat -> Nat -> Nat.-}

data Nat = Zero | Suc Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar Zero n = n
somar (Suc m) n = Suc (somar m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Suc m) n = somar n (mult m n)

{-2. O Prelude define o tipo Ordering

data Ordering = LT | EQ | GT

e a função

compare :: Ord a => a -> a -> Ordering

que decide se o primeiro valor recebido como argumento é menor (LT), igual (EQ) ou maior (GT) que o segundo argumento. Usando essa função redefina a função existe :: Ord a => a -> Arvore a -> Bool para árvores binárias de busca.
-}

data Arvore a = Vazia | No a (Arvore a) (Arvore a)

-- Existe padrão
existe :: Ord a => a -> Arvore a -> Bool
existe x Vazia = False
existe x (No y esq dir)
    | x == y = True
    | x < y = existe x esq
    | otherwise = existe x dir

-- Compare
exista :: Ord a => a -> Arvore a -> Bool
exista x Vazia = False
exista x (No y esq dir) = 
    case compare x y of
        LT -> existe x esq
        EQ -> True
        GT -> existe x dir

{-3. Considere o seguinte tipo de árvores binárias:

data Arvore a = Folha a | No (Arvore a) (Arvore a)

	Digamos que a árvore é balanceada se a quantidade de folhas do lado esquerdo e do lado direito de todos os nós são iguais ou sua diferença é no máximo 1, e suas folhas são consideradas balanceadas por definição. Defina uma função balanceada :: Arvore a -> Bool que decide se uma árvore é balanceada ou não. IMPORTANTE: Uma árvore está balanceada se todas as suas sub-árvores também estão balanceadas. DICA: primeiro defina uma função que conta a quantidade de folhas em uma árvore.
-}

data Arvora a = Folha a | Node (Arvora a) (Arvora a)

folhas :: Arvora a -> Int
folhas (Folha _) = 1
folhas (Node esq dir) = folhas esq + folhas dir

balanceada :: Arvora a -> Bool
balanceada (Folha _) = True
balanceada (Node esq dir)
    | abs (folhas esq - folhas dir) <= 1 = balanceada esq && balanceada dir
    | otherwise = False

{-4. Defina a função balancear :: [a] -> Arvore a que converte uma lista não vazia em uma árvore balanceada. Dica: primeiro defina uma função que divide uma lista em duas metades cujos tamanhos diferem em no máximo 1.-}

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort maiores
    where
        menores = [y | y <- xs, y <= x]
        maiores = [y | y <- xs, y > x]

balancear :: [a] -> Arvore a
balancear [] = Vazia
balancear [x] = No x Vazia Vazia
balancear xs = No x (balancear esq) (balancear dir)
    where (esq, x:dir) = splitAt (length xs `div` 2) xs

{-5. Dada a definição

data Expr = Val Int | Add Expr Expr

defina a função

avaliar :: Expr -> Int

tal que avaliar substitui cada construtor Val na expressão pelo valor Int representado pelo construtor, e cada construtor Add pela aplicação da função (+).-}

data Expr = Val Int | Add Expr Expr

avaliar :: Expr -> Int
avaliar (Val n) = n
avaliar (add v1 v2) = avaliar v1 + avaliar v2

{-6. Utilizando a definição

data Expr = Val Int | Op Expr Expr

defina a função de alta ordem

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

tal que folde f g substitui cada construtor Val na expressão pela aplicação da função f ao valor representado pelo Val, e cada construtor Op pela aplicação da função g aos valores resultantes de ambas as expressões codificadas pelo construtor Op.-}

data Expr = Val Int | Op Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Op v1 v2) = g (folde f g v1) (folde f g v2)

{-7. Usando a função folde, defina a função eval :: Expr -> Int que avalia uma expressão para um valor inteiro. Uma forma de enxergar o que a função eval deve fazer é refletir sobre a seguinte frase: como eu posso usar a função folde de forma que eval avalie a expressão assumindo que Op signifique a soma? Por exemplo, se eu executasse eval (Op (Val 1) (Val 4)), assumindo que Op é a soma, ela deveria retornar 5. Perceba que Op poderia representar qualquer operação sobre os valores, basta que você forneça a operação desejada para a função folde.-}

eval :: Expr -> Int
eval ex = folde id (+) ex
