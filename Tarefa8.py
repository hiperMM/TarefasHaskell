#Usando  os  conceitos  de  programação  funcional  e  considerando  o  universo  dos  números
#inteiros,  implemente  a  função  foldr  em  Python,  C  ou  C++  20  tomando  como  base  o
#funcionamento  desta  função  em  Haskell.  Sem,  é  claro,  usar  qualquer  função,  objeto,  ou
#biblioteca disponíveis na linguagem que você escolher.


def foldr(function, value, list_):
  if list_ == []:
    return value
  else:
    return function(list_[0], foldr(function, value, list_[1:]))


#print(foldr(lambda x,y: x-y,1,[10,11]) )

#------------------------------------------------------------------------------------------------------

#Usando  os  conceitos  de  programação  funcional  e  considerando  o  universo  dos  números
#inteiros, implemente a função abs em Python, C ou C++ 20 que devolva o valor absoluto de
#um número dado. Sem, é claro, usar qualquer função, objeto, ou biblioteca disponíveis na
#inguagem que você escolher.


def ValorAbsoluto(Numero):
  if (Numero >= 0):
    return Numero
  else:
    return -(Numero)


#print(ValorAbsoluto (-899))

#------------------------------------------------------------------------------------------------------


def MediaAritimetica(Numero1, Numero2):
  return (Numero1 + Numero2) / 2


#print(MediaAritimetica(10, 20))

#------------------------------------------------------------------------------------------------------

#Usando os conceitos de programação funcional e a linguagem Python, C ou C++ 20 escreva
#uma  função  que  devolva  a  lista  de  todos  os  números  de  Fibonacci  até  um  valor  dado
#considerando que a  sequência de Fibonacci começa em zero. Sem, é  claro, usar qualquer
#função, objeto, ou biblioteca disponíveis na linguagem que você escolher.


def Fibonacci(Numero):
  if (Numero == 1):
    return 1
  if (Numero == 2):
    return 1
  else:
    return Fibonacci(Numero - 1) + Fibonacci(Numero - 2)


#print(Fibonacci (10))

#Você tem uma lista de tuplas onde o primeiro campo é o nome de um aluno e o segundo
#sua nota. Crie uma função, usando o Python, C ou C++ 20 e os conceitos de programação
#funcional para  criar uma  função que  ordene  listas  de  tuplas,  como  a  tupla  descrita neste
#enunciado.  Sem,  é  claro,  usar  qualquer  função,  objeto,  ou  biblioteca  disponíveis  na
#linguagem que você escolher.


def a(lista):
  if (len(lista) == 0):
    return []
  return a([i for i in lista[1:] if i[1] < lista[0][1]]) + [lista[0]] + a([i for i in lista[1:] if i[1] >= lista[0][1]])


print(a([('joao', 5), ('ana', 6), ('salomao', 9),('beatriz',10),('alguem',6)]))
