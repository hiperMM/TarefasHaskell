--Aluno Matheus Monteiro

--------------------------------------------------------
import Data.List (intercalate, transpose)
--------------------------------------------------------
--exercício 1
{-
Crie uma função chamada tirade que recebe um inteiro i e uma lista e devolva uma lista sem  
o valor que esteja no índice i e sem todos os valores que estejam  em índices múltiplos de i. 
-}

tirade :: Int -> [Int] -> [Int]
tirade 1 lita = []
tirade numero lista = [lista!!x | x <- [0..length lista - 1],  x `mod` numero == 0] 


--------------------------------------------------------
--exercício 2
{-
Crie uma função chamada splitem que recebe um inteiro n e uma lista e devolva uma tupla 
onde o primeiro elemento será composta dos n primeiros elementos da lista de entrada e o 
segundo elemento será composto de todos os outros elementos da lista de entrada.  
-}

splitem :: Int -> [Int] -> ([Int], [Int])
splitem numero lista = splitAt numero lista

--------------------------------------------------------
--exercício 3

{-
A Cifra de Cesar é uma técnica de criptografia inocente que consiste na troca dos caracteres 
utilizados para  escrever uma  mensagem  a  partir de um deslocamento. Desta  forma,  se  o 
deslocamento for de 3 caracteres o A vira D. Sua tarefa será criar uma função que recebe 
um  inteiro  e  uma  string  e  devolve  uma  string  criptografada  segundo  a  Cifra  de  Cesar.  Se 
quiser  saber  mais  sobre  a  Cifra  de  Cesar  leia  o  artigo  disponível  em: 
https://pt.wikipedia.org/wiki/Cifra_de_C%C3%A9sar. 
-}

cifraDeCesarDiferenciada :: Int -> String -> String
cifraDeCesarDiferenciada numero = map listaCaractere
  where
    alfabeto = ['a'..'z']
    ciclarAlfabeto = cycle alfabeto
    listaCaractere caractere = head
        $ drop (length alfabeto + numero)
        $ dropWhile (/= caractere) ciclarAlfabeto

--------------------------------------------------------
-- exercício 4
{-
Uma matriz espiral é uma tabela onde linhas e colunas são preenchidas na forma de uma 
matriz,  da  esquerda  para  direita  e  de  cima para baixo  por  números  inteiros  naturais.  Sua 
tarefa  será  criar  uma  função  chamada  de  espiral  que  recebe  um  inteiro  e  devolve  uma 
matriz espiral conforme os exemplos a seguir:  

_____________________________________
|   matriz 3x3    |    matiz 4x4    |
-------------------------------------
|    1  2  3      |   1  2  3  4    |
|    8  9  4      |   12 13 14 5    |
|    7  6  5      |   11 16 15 6    |
|                 |   10 9  8  7    |
-------------------------------------
-}

espiral :: Int -> [[Int]]
espiral numero = go numero numero 0
  where
    go linha coluna x
      | 0 < linha =
          [x .. pred coluna + x] :
          fmap
            reverse
            (transpose $ go coluna (pred linha) (x + coluna))
      | otherwise = [[]]


--------------------------------------------------------
-- exercício 5
{-
Crie uma função chamada de difquadrado que recebe uma lista com não menos 10 números 
naturais e devolva a diferença entre o quadrado da soma e a soma dos quadrados destes 
números. 
-}

listaQuadrado :: [Int] -> [Int]
listaQuadrado [] = []
listaQuadrado (x:xs) = [x^2] ++ (listaQuadrado xs)


difquadrado :: [Int] -> Int
difquadrado lista = (sum(lista))^2 - sum(listaQuadrado(lista))

--------------------------------------------------------

main = do
---------------------
--questão 1
  print(tirade 3 [1..99])
---------------------
--questão 2
  print(splitem 2 [1,2,3,4,5])
---------------------
--questão 3
  print(cifraDeCesarDiferenciada 1 "abcd")
---------------------
--questão 4
  print(espiral 5)
---------------------
--questão 5
  print(difquadrado [1,2,3,4])
