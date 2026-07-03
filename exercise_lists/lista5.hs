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
