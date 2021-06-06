-- Teoria da Computação - Haskell (Atividade 2)
-- 01
todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a==b) && (b==c)

-- 02
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = not ((a==b) && (b==c))

-- 03
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
 | (a==b) && (b==c) = 3
 | (a==b) || (b==c) = 2
 | not ((a==b) && (b==c)) = 0

-- 04
multiplicacao :: Int -> Int -> Int
multiplicacao a b
 | b == 0 = 0
 | b > 0 = a + multiplicacao a (b - 1)

-- 05
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c)
 |(a<=b) && (a<=c) && (b<=c) = (a, b, c)
 |(b<=a) && (b<=c) && (a<=c) = (b, a, c)
 |(c<=a) && (c<=b) && (a<=b) = (c, a, b)
 |otherwise = (c, b, a)

-- 06
matches :: Int -> [Int] -> [Int]
matches a lista = [x | x <- lista, x == a]
 
-- 07
elem_ :: Int -> [Int] -> Bool
elem_ a lista
 | matches a lista == [] = False
 | otherwise = True 

-- 08.1
remover :: Int -> [Int] -> [Int]
remover a [] = []
remover a (x:xs)
 |x == a = remover a xs
 |otherwise = x:remover a xs

-- 08.2
menor :: [Int] -> Int
menor [x] = x
menor (x:xs)
 |x < menor xs = x
 |otherwise = menor xs

-- 08.3
ordena :: [Int] -> [Int]
ordena [] = []
ordena lista = x : ordena (remover x lista)
 where
  x = menor lista

-- 09
maiorDuasTuplas :: (Int, Int) -> (Int, Int) -> Int
maiorDuasTuplas (a, b) (c, d)
 |(a > b) && (a > c) && (a > d) = a
 |(b > a) && (b > c) && (b > d) = b
 |(c > a) && (c > b) && (c > d) = c
 |otherwise = d

-- 10
separaLista :: Int -> [Int] -> ([Int], [Int])
separaLista a lista = ([x|x<-lista,x<a], [x|x<-lista,x>=a]) 

-- 11
maiorElemento :: [Int] -> Int
maiorElemento [x] = x
maiorElemento (x:xs)
 |x > maiorElemento xs = x
 |otherwise = maiorElemento xs

main = do
 print("01",todosIguais 1 1 1)
 print("01",todosIguais 1 2 3)
 print("02",todosDiferentes 1 1 1)
 print("02",todosDiferentes 1 2 3)
 print("03",quantosIguais 1 1 1)
 print("03",quantosIguais 1 1 2)
 print("03",quantosIguais 1 2 3)
 print("04",multiplicacao 2 3)
 print("04",multiplicacao 2 0)
 print("05",ordenaTripla (3,4,1))
 print("06",matches 1 [1,2,1,4,5,1])
 print("06",matches 3 [2,1,4,6])
 print("07",elem_ 1 [2,3,4])
 print("07",elem_ 1 [1,2,3,1])
 print("08.1",remover 1 [1,2,3,1])
 print("08.2",menor [1,2,3,1])
 print("08.3",ordena [5,3,1,4,2])
 print("08.3",ordena [5,4,3,2,1])
 print("08.3",ordena [1,2,3,4,5])
 print("09",maiorDuasTuplas (1,2) (3,4))
 print("09",maiorDuasTuplas (4,3) (2,1))
 print("09",maiorDuasTuplas (3,2) (4,1))
 print("09",maiorDuasTuplas (1,4) (2,3))
 print("10",separaLista 4 [1,2,3,4,5,6])
 print("11",maiorElemento [5,6,8,7,2,3,4])