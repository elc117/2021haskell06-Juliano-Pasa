-- Prática 03 de Haskell
-- Nome: Juliano de Mello Pasa

-- Exercicio 1
-- Ends

ends :: [Int] -> [Int]
ends x = head x : last x : []

-- Exercicio 2
-- Funcao deduzame

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : (deduzame xs)

-- Exercicio 3
-- Funcao deduzame2

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs

-- Exercicio 4
-- Eleva ao quadrado

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela x = (x, x^2) : geraTabela (x-1)

-- Exercicio 5
-- Confere caracter

contido :: Char -> String -> Bool
contido _ "" = False
contido c (x:xs) = (c == x) || (contido c xs)

-- Exercicio 6
-- Soma 2

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x, y):xs) = (x+2, y+2) : translate xs 

-- Exercicio 7
-- Conta gigantes
-- fromEnum transforma retorna 1 para True e 0 para False

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = (fromEnum $ (length x) > 5) + countLongs xs  

-- Exercicio 8
-- Somente gigantes

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs)
  | length x > 5 = x : onlyLongs xs
  | otherwise = onlyLongs xs




