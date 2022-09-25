--Tarefa 5 | aluno: Matheus Monteiro
----------------------------------------------------------------------------
--Exercício 1
{-
Em Haskell temos um conjunto de tipos especiais: Maybe, Nothing e Just seu trabalho será 
criar no repl.it um código, comentado, que explique estes tipos, sua funcionalidade e que 
contenha  quatro  exemplos  do  seu  uso.  Com  uma  restrição:  você  deve  evitar  qualquer 
explicação que inclua o uso de Monads. Lembre-se entre estes exemplos, um deve ser de 
sua autoria e todos os outros precisam ter suas fontes citadas. 
-}

--Maybe: a função Maybe funciona para ver se um valor esta ou não presente

--Just: o just é um construtor da função Maybe ele verifica se o valor esta presente, se ele achar, retorna o valor

--Nothing: o nothing é um construtor do Maybe ele representa se o valor não for encontrado, então ele pode devolver o valor em si, ou nada

--Exemplos:
--(maybe e just)
value1 = 1
value2 = 2
value3 = 3
--exemplo meu
valor = 4
valor1 = 5

--https://www.educba.com/haskell-maybe/

-------------- continuação na função main -------------------------------------
--(maybe e nothing)
-- exemplo meu
divisão :: Int -> Int -> Maybe Int
divisão _ 0 = Nothing
divisão x y = Just (div x y)


------------------------------------------------------------------------------------
--http://www.sfu.ca/~tjd/383summer2019/haskell_basic_user_types_lhs.html
safeInvert :: Float -> Maybe Float
safeInvert 0.0 = Nothing
safeInvert x   = Just (1.0 / x)

maybeAdd :: Maybe Float -> Maybe Float -> Maybe Float
maybeAdd Nothing _         = Nothing
maybeAdd _ Nothing         = Nothing
maybeAdd (Just x) (Just y) = Just (x + y)

maybeSum :: [Maybe Integer] -> Integer
maybeSum []            = 0
maybeSum (Nothing:xs)  = maybeSum xs
maybeSum ((Just x):xs) = x + (maybeSum xs)

--https://wiki.haskell.org/Maybe

----------------------------------------------------------------------------

{-
2. Escreva uma função chamada idade que usando pelo menos um tipo definido por você que 
receba o tempo de vida em segundos de uma determinada pessoa, o nome de um planeta 
e devolva a idade desta pessoa em anos caso ela tivesse vivido naquele planeta. Sabendo 
que o período orbital dos planetas é dado por:  
a. Mercúrio: 0.2408467 anos terrestres; 
b. Vênus: 0.61519726 anos terrestres; 
c. Terra: 1.0 anos terrestre equivalente a 365.25 dias, ou 31.557.600 segundos; 
d. Marte: 1.8808158 anos terrestres; 
e. Jupiter: 11.862615 anos terrestres; 
f. Saturno: 29.447498 anos terrestres; 
g. Urano: 84.016846 anos terrestres; 
h. Netuno: 164.79132 anos terrestres; 
-}


type Tempo = Float

anosTerrestres :: Tempo -> Tempo
anosTerrestres a = (/) a 31557600

mercurio :: Tempo -> Tempo
mercurio a = anosTerrestres(a) * 0.2408467

venus :: Tempo -> Tempo
venus a = anosTerrestres(a) * 0.61519726

marte :: Tempo -> Tempo
marte a = anosTerrestres(a) * 1.8808158

jupiter :: Tempo -> Tempo
jupiter a = anosTerrestres(a) * 11.862615

saturno :: Tempo -> Tempo
saturno a = anosTerrestres(a) * 29.447498

urano :: Tempo -> Tempo
urano a = anosTerrestres(a) * 84.016846

netuno :: Tempo -> Tempo
netuno a = anosTerrestres(a) * 164.79132

----------------------------------------------------------------------------

{-
Dada uma coleção de números, implemente as funções manter e descartar que recebem a 
coleção e um predicado. Sempre que o predicado for verdadeiro quando aplicado a um item 
da  coleção  original  este  item  deve  ser  mantido  ou  descartado.  Lembre-se  dos  valores 
imutáveis.  Você  não  pode  usar  as  funções  filter  e  reject  já  disponíveis  no  Prelude  ou  em 
qualquer outra biblioteca.  
-}

manter :: [Int] -> (Int -> Bool) -> [Int]
manter lista predicado = [x | x <- lista, predicado x]

descartar :: [Int] -> (Int -> Bool) -> [Int]
descartar lista predicado = [x | x <- lista, not(predicado x)]

----------------------------------------------------------------------------
{-
Um dos jogos infantis mais populares no planeta, chamado de Jogo Da Forca, consiste na 
escolha  de  letras  do  alfabeto  latino  para  tentar  encontrar  uma  palavra  que  esteja  oculta. 
Você  deve  implementar  uma  versão  deste  jogo  em  Haskell  considerando  as  seguintes 
restrições:  você  deverá  usar  caracteres  ASCII  para  representar  a  forca,  o  enforcado,  e  os 
espaços selecionados para cada palavra; os símbolos usados nas palavras serão apenas os 
caracteres  do  alfabeto  latino  minúsculos  entre  a  e  z;  o  banco  de  dados  de  palavras  deve 
conter, no mínimo 10 palavras de 6 letras ou mais; a cada tentativa a tela precisa ser limpa 
e redesenhada (ANSI Escape Codes · GitHub). No repl.it não é possível importar a biblioteca 
responsável  pela  geração  de  números  randômicos,  pode  gerar  as  palavras  e  usá-las  em 
sequência  ou,  se  preferir,  use  algum  fator  do  sistema,  como  os  últimos  dígitos  da  data 
corrente (Dates and Times (sourceforge.net)) 
-}

----------------------------------------------------------------------------
main = do
----------------------------------------------------------------------------
  let result1 = maybe False odd (Just value1)
  print("result for value 1 is :", result1)

  let result2 = maybe False odd (Just value2)
  print("result for value 2 is :", result2)

  let result3 = maybe False odd (Just value3)
  print("result for value 3 is :", result3)

----------------------------------------------------------------------------
 --exmplos meus
  let resultado = maybe False odd(Just valor)
  print(resultado)

  let resultado1 = maybe False odd (Just value1)
  print("result for value 1 is :", resultado1)

  print(divisão 5 0)
  print(divisão 10 2)
  print("--------------------------------------------------------------")
----------------------------------------------------------------------------
  print(safeInvert 4)
  print(safeInvert 0)

  print(maybeAdd (safeInvert 2) (safeInvert 2))
  print(maybeAdd (safeInvert 2) (safeInvert 0))

  print(maybeSum [Just 3, Just 6, Nothing, Just 2])

  print("--------------------------------------------------------------")
----------------------------------------------------------------------------

  print("a idade seria:",netuno 3155760000)

  print("--------------------------------------------------------------")
----------------------------------------------------------------------------
  print(manter [2 .. 10] (\x -> x `mod` 2 == 0))
  print(descartar [ 1 .. 10] (\x -> x `mod` 2 == 0))
  

