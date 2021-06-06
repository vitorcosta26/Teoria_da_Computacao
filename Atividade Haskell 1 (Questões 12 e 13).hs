-- Atividade Haskell 1 (Questões 12 e 13)
-- 12
listaMaisN :: [Int] -> Int -> [Int]
listaMaisN [] n = [n]
listaMaisN (x:xs) n = x:listaMaisN xs n

-- 13
soma_tamanho :: [Int] -> (Int, Int)
soma_tamanho [] = (0, 0)
soma_tamanho (x:xs) = (1 + a, x + b)
 where
  (a, b) = soma_tamanho xs

--------------------------------------------------
main = do
 print("12", listaMaisN [1,2,3] 4)
 print("13", soma_tamanho [1,2,3])