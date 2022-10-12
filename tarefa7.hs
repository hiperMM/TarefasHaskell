-- Aluno: Matheus Monteiro
--Tarefa 7

------------------------------------------------------------------------
--importações

import Data.List (genericReplicate)

------------------------------------------------------------------------
{-
Exercício 1

1. Utilizando a linguagem Haskell e o seu próprio tipo de dados, crie um programa capaz de 
classificar triângulos a partir do comprimento dos seus lados sabendo que o comprimento 
de cada um dos lados deve ser maior que zero e que:  
  a. triângulos equiláteros têm todos os lados do mesmo tamanho; 
  b. triangulos esosceles têm, no mínimo, dois lados do mesmo tamanho;  
  c. triangulos escalenos têm tem todos os lados de tamanho diferentes; 
  d. triangulos degenerados têm um lado igual a soma dos outros dois e área zero.
-}

type Lado = Float 

verificarTriangulo :: Lado -> Lado -> Lado -> String 
verificarTriangulo a b c
                        | and(a == b, a ==c) = "equilatero"
                        | or[a == b, b == c, a == c] = "isoceles"
                        | or[a /= b, b /= c, a /= c] = "escaleno"
                        | or[(a == (b + c)), (b == (a + c )), (c == (a + b))] = "degenerado"
                        | otherwise = "n foi reconhecido"

------------------------------------------------------------------------
{-
Usando Haskell, crie uma função chamada fatias, com a assinatura dada por fatias:: :: Int -> 
String -> [[Int]] que receba um string e um inteiro e devolva uma lista de listas contendo em 
cada  item  uma  lista  de  inteiros.  Esta  função  receberá  strings  contendo  digitos  como,  por 
exemplo:  "345234678"  e  devolverá  listas  parecidas  com 
[[3,4,5],[4,5,2],[5,2,3],[2,3,4],[3,4,6],[4,6,7],[6,7,8]] No caso do exemplo, o inteiro que fatias 
recebeu foi 3. Observe que você poderá criar, quantas funções de apoio acredite que sejam 
necessárias  para  criar  as  funcionalidades  de  fatias  inclusive,  se  achar  interessante,  pode 
usar as funções mapMaybe e digitToInt.  
-}


{-
transformaInteiro :: Char -> Int
transformaInteiro c = fromEnum c - fromEnum '0' 

fatias:: Int -> String -> [[Int]]
--condição de parada
fatias n palavra
  |n < length palavra = []
  |otherwise = transformaInteiro(take n palavra) : transformaInteiro(fatias n (tail palavra))

listaFinal:: String -> [Int]
listaFinal palavra
    | null palavra = []
    | otherwise = transformaInteiro(head palavra) :  listaFinal (tail palavra)

-}

------------------------------------------------------------------------
{-
3. Usando  Haskell  escreva  uma  função  chamada  romanos  que  receba  um  inteiro  menor  ou 
igual a 3000 e devolva um string deste inteiro representado com algarismos romanos.  
-}

listaAlgarismos = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
                   (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
                   (4,"IV"), (1,"I")]

romano :: Integer -> String
romano 0 = "não existe"
romano numero | numero > 0 = snd $ foldl predicato (numero,[]) listaAlgarismos
  where predicato (n,s) (rn, rs) = (l, s ++ concat (genericReplicate k rs))
              where (k,l) = divMod n rn
------------------------------------------------------------------------
--Questão 4
{-
4. Usando linguagem  Haskell, escreva uma função que recebe uma lista de listas de inteiros 
com  até  5  digitos  em  cada  lista  e  devolva  apenas  as  listas  que  contenham  palíndromes 
primos.  Por  exemplo  na lista  de  listas  [[1,1,1],  [1,2,2],  [3,1,3],  [3,1,5]]  apenas  o  elemento 
[3,1,3] é um palíndrome  primo. Uma ferramenta importante para criar suas listas de teste 
pode ser encontrada em: Prime Number Calculator (calculatorsoup.com). 
-}

numeroLista :: [Int] -> Int
numeroLista lista = sum(reverse([lista!!(x-1) * 10^(x-1) | x <- [1..length lista]]))

ehprimo :: Int -> Bool
ehprimo numero = length [x | x <- [1..numero],numero `mod` x == 0] == 2


palindromo :: [[Int]] -> [[Int]]
palindromo lista =  [x | x <- lista, x == reverse(x), ehprimo (numeroLista x)]


------------------------------------------------------------------------
{-
Usando a linguagem Haskell escreva uma função, chamada ultimoNome que receba o nome 
completo de uma pessoa e devolva apenas o último sobrenome sem qualquer vogal. Caso 
o  ultimo  sobrenome  não  contenhuma  nenhuma  vogal  devolva  o  ultimo  sobrenome  que 
ainda contenha vogal. Por exemplo se o nome for Ana Maria stzrx, a função deve devolver 
Maria, se o nome for Silvia Silva a função deve devolver Slv.   
-}

ultimoNome :: String -> String
ultimoNome nome
              |length (last (words(nome))) == length(filter (\x -> x /= 'a' && x /= 'e' && x /= 'i' && x /= 'o' && x /= 'u') (last (words(nome))))= last(init(words(nome)))
              |otherwise = filter (\x -> x /= 'a' && x /= 'e' && x /= 'i' && x /= 'o' && x /= 'u') (last (words(nome)))



------------------------------------------------------------------------
main = do
--Questão 1
  print(verificarTriangulo 3 3 3)
  print(verificarTriangulo 2 3 3)
  print(verificarTriangulo 2 3 4)
  print(verificarTriangulo 3 7 10)
----------------------------------------
--Questão 2
----------------------------------------
--Questão 3
  print(romano 19)
----------------------------------------
--Qustão 4
  print(palindromo [[1,1,1],  [1,2,2],  [3,1,3],  [3,1,5]])

--Questão 5
  print(ultimoNome "Matheus Monteiro")
