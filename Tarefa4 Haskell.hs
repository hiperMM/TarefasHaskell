--Tarefa 4 | aluno: Matheus Monteiro
--------------------------------------------------------------------------------------
--Exercício 1
{-
  Escreva uma função chamada fatorialn que usando o operador range e a função
foldr devolva o fatorial de n. 
-}
numero :: Int -> [Int]
numero 0 = []
numero n = reverse [1 .. n]

fatorialn :: [Int] -> Int
fatorialn numero = foldr (*) 1 numero

--------------------------------------------------------------------------------------
--Exercício 2
{-
  Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de 
números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos 
reais listados. 
-}

quadradoReal :: [Float] -> [Float]
quadradoReal (head: tail) = map (^2) (head:tail)

--------------------------------------------------------------------------------------
--Exercício 3
{-
  Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de 
palavras e devolve uma lista com o comprimento de cada uma destas palavras. 
-}


comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras (head:tail) = map (length) (head:tail)

--------------------------------------------------------------------------------------
--Exercício 4
{-
Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior 
número entre 0 e 100000 que seja divisivel por 29.
-}

multiploDe29 :: Int -> Bool
multiploDe29 1 = False
multiploDe29 n
              | mod n 29 == 0 = True
              | otherwise = False

maiorMultiploDe29 :: [Int] -> Int
maiorMultiploDe29 (head:tail) = last (filter multiploDe29 (head:tail))
--------------------------------------------------------------------------------------
--Exercício 5
{-
Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um 
inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. 
-}
multiploDe :: Int -> Int -> Bool
multiploDe n 0 = False
multiploDe n x = if mod x n == 0
  then True
  else False

maiorMultiploDe :: (Int -> Bool) -> [Int] -> Int
maiorMultiploDe multiploDe (head:tail) = last (filter multiploDe (head:tail))
--------------------------------------------------------------------------------------
--Exercício 6
{-
Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva 
a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De 
tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1^2 +2^2 +3^2 +4^2...+𝑛^2. 
-}


listaQuadrados :: [Int] -> [Int]
listaQuadrados (head:tail) = map (^2) (head:tail)

somaQuadrados :: ([Int] -> [Int]) -> [Int] -> Int
somaQuadrados listaQuadrados(head:tail) = foldr (+) 0 (listaQuadrados(head:tail))
--------------------------------------------------------------------------------------
--Exercício 7
{-
 Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o 
comprimento (cardinalidade) de uma lista dada. 
-}

comprimento :: [a] -> Int
comprimento = foldl (\n _ -> n+1) 0

--------------------------------------------------------------------------------------

main = do
----------------------------------------
--exercício1
  print(fatorialn (numero 10))
--exercício2
  print(quadradoReal [12,4,3])
--exercício 3
  print(comprimentoPalavras ["so", "sei", "que", "nada", "sei"])
--exercício 4
  print(maiorMultiploDe29[0 .. 100000])
--exercício 5
  print(maiorMultiploDe (multiploDe 30) [0 .. 100000])
--exercício 6
  print(somaQuadrados listaQuadrados[1 .. 10])
--exercício 7
  print(comprimento [1,2,3,4,5,33,56])
