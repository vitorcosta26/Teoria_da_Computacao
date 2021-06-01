-- Teoria da Computação - Haskell (Atividade 1)
-- 1
fatorialDuplo :: Int -> Int
fatorialDuplo n
 | n == 0 = 1
 | n == 1 = 1
 | n > 1 = n * fatorialDuplo (n - 2)

-- 2
produtoIntervalo :: Int -> Int -> Int
produtoIntervalo a b
 | a > b = 1
 | otherwise = a * produtoIntervalo (a + 1) b

-- 3
fatorial :: Int -> Int
fatorial n = produtoIntervalo 1 n

-- 4
soma :: Int -> Int -> Int
soma a b
 | b == 0 = a
 | otherwise = soma (succ a) (pred b)

-- 5
potencia :: Int -> Int -> Int
potencia x n
 | n == 0 = 1
 | otherwise = x * potencia x (n-1)

-- 6
quociente :: Int -> Int -> Int
quociente a b
 | a < b = 0
 | otherwise = 1 + quociente (a - b) b

resto :: Int -> Int -> Int
resto a b
 | a < b = a
 | otherwise = resto (a - b) b

-- 7
fibonacci_ :: Int -> Int -> Int -> Int
fibonacci_ n a b
 | n == 0 = a
 | n == 1 = b
 | n >= 2 = fibonacci_ (n-1) b (a+b)

fibonacci :: Int -> Int
fibonacci n = fibonacci_ n 0 1


-- 8
produtoLista :: [Int] -> Int
produtoLista [] = 0
produtoLista (x:xs) = x * produtoLista xs

-- 9
listaMaiorN :: [Int] -> Int -> [Int]
listaMaiorN lista n = [x | x <- lista, x > n]

-- 10
intercala :: [Int] -> [Int] -> [Int]
intercala x [] = x
intercala [] y = y
intercala (x:xs) (y:ys) = x: y: intercala xs ys

-- 11
distancia :: [(Float, Float)] -> [Float]
distancia [] = []
distancia ((x,y):xys) = (sqrt (x^2 + y^2)) : distancia xys

-- 12
listaMaisN :: [Int] -> Int -> [Int]
listaMaisN [] n = [n]
listaMaisN (x:xs) n = (x:xs) ++ [n]

-- 13
tamanho_ :: [Int] -> Int
tamanho_ [] = 0
tamanho_ (x:xs) = 1 + tamanho_ xs

soma_ :: [Int] -> Int
soma_ [] = 0
soma_ (x:xs) = x + soma_ xs

soma_tamanho :: [Int] -> (Int, Int)
soma_tamanho [] = (0, 0)
soma_tamanho (x:xs) = (1 + tamanho_ xs, x + soma_ xs)

---------------------------------------------------
main = do
 print(fatorialDuplo 8)
 print(produtoIntervalo 5 1)
 print(fatorial 5)
 print(soma 2 3)
 print(potencia 3 2)
 print(quociente 6 2)
 print(resto 6 2)
 print(fibonacci 7)
 print(produtoLista [1,2,3])
 print(listaMaiorN [4,3,2,1] 2)
 print(intercala [1,2,3] [4,5,6])
 print(distancia [(1, 2), (3, 4), (1, 3), (5, 6)])
 print(listaMaisN [1,2,3] 4)
 print(soma_tamanho [1,2,3])
