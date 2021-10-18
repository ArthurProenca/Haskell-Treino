{------------------------------------------ 
Prova 01 Programação Funcinonal 07/07/2021
-------------------------------------------
Nome completo 01: Arthur Rodrigues Proença
Matricula 01: 2020.1.08.005

Nome completo 02: Gustavo Marquesani da Costa
Matrícula 02: 2020.1.08.011

-------------------------------------------
Instruções para o preenchimento do script
a) Implemente o código na sequência em que aparece na prova
b) separe cada questão com as linhas pontilhadas (abaixo)
   {--Questão X -----------------------------------------------}
c) Questões com mais de um item, separe-os da seguinte forma:

{--Questão X ----------------------------------------------}

--Item X.a
 código da questão X item a

--Item X.b
  código da questão X item b

IMPORTANTE: Todo o script deve ter as citações de todas as questões e itens.

Então, as questões ou itens não solucionados por você deverão receber, impreterivelmete, o texto --EM BRANCO --
Caso você não organize a prova como indicado, poderá ser penalizado na nota final
  
d) Não use nomes de funções diferntes dos indicados nas qeustões.
De forma alternativa, e caso o nome não seja sugerido, dê preferência por começar como: funcQx, em que x refere-se à questão.
Ex: funcQ3a pode ser a implementação da questão 3, item a

e) não é necessário (mas não é um problema) incluir o enunciado das questões no script
f) Você pode inserir comentários pessoais explicando o código implementado antes do cabeçalho da função

g) QUESTÕES COM TÉCNICAS NÃO APRESENTADAS EM AULA NÃO SERÃO CONSIDERADAS


-------------------------------------------}

-- +++++++++++++++++ comece, aqui, sua prova. Boa prova +++++++++++++++++++++++ --

import Data.Char --Necessário para utilização das funções chr e ord neste script. Foi aprendida em aula.

--EM ALGUMAS FUNÇÕES, A DOCUMENTAÇÃO ABAIXO SE ENCONTRA. ELA SERVE PARA EXPLICAÇÃO DO QUE ESTÁ ACONTECENDO.
{-
    @param: Parâmetro(s) da função.
    @explain: Explicação da função.
    @demonstrate: Uma demonstração de execução.
    @return: Retorno.

-}

{--Questão 1 ----------------------------------------------}

--Item 1.a
funcQ1a :: Float -> Float
funcQ1a x
    | x < 0 = (/) 2.0 x
    | otherwise = (/) (x+4.0) (x+2.0)

--Item 1.b
funcQ1b :: Float -> Float -> Float
funcQ1b x y
    | x >= y = x + y
    | otherwise = x - y

--Item 1.c
funcQ1c :: Float -> Float -> Float -> Float
funcQ1c x y z
    | (x + y) > z = x + y + z
    | (x + y) < z = x - y - z
    | otherwise = 0

{--Questão 2 ----------------------------------------------}
{-
É necessário um caso base, uma condição inicial, para que não haja problemas como multiplicação por números negativos) no fatorial. 
No caso, o valor da chamada fat 0 é 1.
Perceba que caso fat 0 não fosse definida, quando x = 0, não seria respeitada também a proposta (ir até um número x dado pelo usuário).
-}
fat :: Int -> Int
fat 0 = 1
fat x = x * fat(x-1)

{--Questão 3 ----------------------------------------------}
{-
    @param: Int, Int
    @explain: Realiza a multiplicação usando a função soma. Basicamente, somamos 0 os dois parâmetros para podermos multiplicá-los separadamente.
    @demonstrate: multiplica 2 10 = 20
    @return: Multiplicação entre os dois parâmetros.

-}
soma :: Int -> Int -> Int
soma x y = x + y

multiplica :: Int -> Int -> Int
multiplica x y = (soma x 0) * (soma 0 y)

{--Questão 4 ------------------------------------------------------}
{-
    @param: Int
    @explain: Inverte um inteiro pelo método da soma de sua divisão por 10 com o resto (multiplicado por 10 posteriormente) de sua divisão por 10. 
    Esse método se assemelha a pegar como aux seu resto, onde sempre que seu número for menor que 9, dividirá sucessivamente.
    @demonstrate: reverseNumber -23 = -(reverseNumber(23)) = 23 0 = (funQ4 ((2)) ((0) + (3))) = 2 3 = (0) ((30) + 2)) = 0 32, retornando -32 à reverseNumber. 
    @return: Número invertido.

-}
funcQ4 :: Int -> Int -> Int
funcQ4 0 invertido = invertido --Invertido sempre recebe a soma do (resto*10) + resto atual, até o número não ser mais divisível por 10.
funcQ4 num invertido = (funcQ4 ((div) num 10) ((invertido*10) + ((rem) num 10)))

reverseNumber :: Int -> Int
reverseNumber num
    | (num < 0) = ((-1) * reverseNumber ((-1) * num)) --Multiplica o retorno (convertido para positivo) em negativo, mantendo o digitado pelo usuário.
    | otherwise = funcQ4 num 0

{--Questão 5 ------------------------------------------------------}
{-
    @param: Int
    @explain: Usaremos o recurso (x^y)*(x^y) = x^y*y.
    @demonstrate: 2^2*2^2 = 2^4
    @return: Função que retorna x^4. 

-}

square:: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = (square x) * (square x)

{--Questão 6 ------------------------------------------------------}
{-
    @param: Int'ésimo' termo da série.
    @explain: Recursivamente tira a raiz de 6 'x' vezes 
    @demonstrate: x = 5 = sqrt(6+(sqrt(4 + sqrt(3 + sqrt(2 + sqrt(1)))))).
    @return: N-ésima raiz de 6 recursiva.

-}
iSqrt6 :: Int -> Float
iSqrt6 0 = sqrt(6)
iSqrt6 x = sqrt(6 + iSqrt6(x-1))

{--Questão 7 ------------------------------------------------------}
{-
    @param: Int, Int
    @explain: Utilizando a fórmula de permutação e o fatorial (criado na questão 2), podemos usar apenas uma comparação para determinar as formas.
    @demonstrate: 3!/2!*(3-2)! = 3.
    @return: Quantos modos podemos organizar p em n
-}

funcQ7 :: Int -> Int -> Int
funcQ7 n p 
    | n>=p = (div) (fat n) ((fat p)*fat(n-p))
    | otherwise = 0

{--Questão 8 ------------------------------------------------------}
{-
    @param: Int, Int
    @explain: Resto da divisão entre b e (resto da divisão entre a b). Dessa forma, o maior divisor entre os mesmos será o que não chegar a 1 primeiro.
    @demonstrate: 12%4 = 0, retorna 4. 15%4 = 3 e 4%3 = 1.
    @return: Máximo divisor comum entre dois inteiros.
    @OBSERVAÇÃO: Esta função utiliza o Algoritmo de Euclides para determinar MDC entre dois números, conforme visto na lista de Prolog.
-}
mdc :: Int -> Int -> Int
mdc 0 b = b --Condição de base 1.
mdc a 0 = a --Condição de base 2.
mdc a b = mdc b ((rem) a b)

{--Questão 9 ------------------------------------------------------} 
{-
    @param: Int, Int, Int, Int, Int
    @explain: Resto da divisão do número é 0, é múltiplo.
    @demonstrate: howManyMultiples 4 1 10 = funQ9 4 1 10 0 1 = 
    @return: 
-}
funcQ9 :: Int -> Int -> Int -> Int -> Int -> Int
funcQ9 n inicio_intervalo fim_intervalo cont inicio_intervalo2
    | ((rem n inicio_intervalo) == 0) && (inicio_intervalo <= fim_intervalo) = funcQ9 n (inicio_intervalo + 1) fim_intervalo (cont + 1) inicio_intervalo2
    | ((rem n inicio_intervalo) /= 0) && (inicio_intervalo <= fim_intervalo) = funcQ9 n (inicio_intervalo + 1) fim_intervalo cont inicio_intervalo2
    | otherwise = (cont - inicio_intervalo2)

howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples n inicio_intervalo fim_intervalo
    | ((rem) n inicio_intervalo == 0) = funcQ9 n inicio_intervalo fim_intervalo (inicio_intervalo-1) inicio_intervalo
    | ((rem) n inicio_intervalo /= 0) = funcQ9 n inicio_intervalo fim_intervalo inicio_intervalo inicio_intervalo

{--Questão 10 ------------------------------------------------------} 
{-
    @param: Int
    @explain: Último digito pode sempre ser obtido através do resto da divisão de um número por 10. Caso seja negativo, calcula para positivo e retorna negativo.
    @demonstrate: x = 30 = 30%10 = 0 = 3%10 = 3 = 03.
    @return: 

-}
lastDigit :: Int -> Int
lastDigit x
    | x < 0 = (rem) ((-1) * x) 10
    | otherwise = (rem) x  10 

{--Questão 11 ------------------------------------------------------} 
{-
    @param: Int, [Int]
    @explain: Estratégia recomendada consiste em transformar o Int digitado em uma lista de caracteres, retornando o 'n-ésimo' termo da lista!
    @demonstrate: Se receber lista vazia, retorna 1. Senão,
    @return: 

-}
decrementaLista :: Int -> [Int] -> Int
decrementaLista index [] = 1
decrementaLista index (cabeca:cauda)
    | index == 0 = cabeca
    | otherwise = decrementaLista (index-1) cauda

anyDigit :: Int -> Int -> Int
anyDigit index number = decrementaLista index (number:[])

{--Questão 12 ------------------------------------------------------} 

--Item 12.a
{-
O que há de errado na definição é sua comparação. M ser diferente de N não implica em M diferente de P. Logo, não compara
se todos são diferentes, apenas se n diferente de m e p.
-}

--Item 12.b
--Esta função, agora, compara se m e p são diferentes. Se todos forem, retorna True
allDifferent :: Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)

{--Questão 13 ------------------------------------------------------} 
{-
    @param: Int, Int, Int
    @explain: Compara de 3 números são iguais e atribui "peso" aos mesmos, sendo 3 se forem iguais, 2 se forem 2 forem iguais e 1 diferente e 0 caso sejam diferentes.
    @demonstrate: howManyEqual 3 1 3 = (False) || (True) || (False) = 2.
    @return: Peso 3 se iguais, peso 2 se dois iguais e peso 0 se diferentes.

-}
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual n1 n2 n3
    | (n1 == n2) && (n2 == n3) = 3
    | ((n1 == n2) && (n2 /= n3)) || ((n1 == n3) && (n2 /= n3)) || ((n1 /= n2) && (n2 == n3)) = 2
    | otherwise = 0

{--Questão 14 ------------------------------------------------------} 

-- define o período de recursão.
periodo :: Int
periodo = 7

-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

--Item 14.a
{-
    @param: Int, Int, Int, Int
    @explain: A função irá buscar quantos valores foram menores que um parâmetro enquanto existir vendas no periodo (incrementando seu índice). 
    Caso a venda seja maior porém período ainda pode ser incrementado, a função somente não incrementa o contador (que denota) quantos valores existem.
    @demonstrate: howManyLess 41 0 7 = auxhMl 41 0 7 0 = 41 0 7 1 = 41 1 7 2 = 41 2 7 2 ... =  41 7 7 3.
    @return: Quantas vezes uma venda foi menor que um valor em dado período.

-}
funcQ14a :: Int -> Int -> Int -> Int -> Int
funcQ14a v ib ie cont
    | (ib <= ie) && (vendas ib < v) = funcQ14a v (ib + 1) ie (cont + 1)
    | (ib < ie) && (vendas ib >= v) = funcQ14a v (ib + 1) ie cont
    | otherwise = cont

howManyLess :: Int -> Int -> Int -> Int
howManyLess value interval_beginning interval_ending = funcQ14a value interval_beginning interval_ending 0

--Item 14.b
{-
    @param: Int
    @explain: Lógica semelhante à questão 14.a, porém, ao invés de retornar número de valores, retorna de número é 0.
    @demonstrate: noZeroInPeriod 3 = funcQ14b 3 0 = 2 0 = 1 0 = 0 1.
    @return: True se não há vendas iguais a 0 no período.

-}
funcQ14b :: Int -> Int -> Bool
funcQ14b day cont
    | (day >= 0) && (vendas day == 0) = funcQ14b (day - 1) (cont + 1)
    | (day >= 0) && (vendas day /= 0) = funcQ14b (day - 1) cont
    | otherwise = (cont == 0)

noZeroInPeriod :: Int -> Bool 
noZeroInPeriod day = funcQ14b day 0

--Item 14.c
{-
    @param: 
    @explain: Concatena os dias onde a venda foi 0 em uma lista de tamanho periodo (pré definido).
    @return: Lista com os dias onde venda foi 0.

-}
auxzIP :: Int -> [Int]
auxzIP 0 = [0]
auxzIP d
    | (d >= 0) && (vendas d == 0) = d : auxzIP (d-1)
    | (d >= 0) && (vendas d /= 0) = auxzIP (d-1)

zerosInPeriod :: [Int] 
zerosInPeriod = auxzIP periodo

--Item 14.d
{-
    @param: Int
    @explain: Dado um valor, monta uma lista com o dia cujo o valor foi maior que a mesma, concatenando o dia com o resultado da função decrementando o dia. 
    Caso contrário, apenas decrementa o dia
    @demonstrate: funcQ14d 10 = (d:funcQ14_d 10 6 = funcQ14_d 10 5 = funcQ14_d 10 4), produzindo uma lista ao final.
    @return: Lista de dias cuja venda atingiu valor menor que o parâmetro.

-}
funcQ14d_2 :: Int -> Int -> [Int]
funcQ14d_2 value 0 = [0]
funcQ14d_2 value day
    | (day > 0) && (vendas day < value) = day : funcQ14d_2 value (day-1)
    | (day > 0) && (vendas day >= value) = funcQ14d_2 value (day-1)

funcQ14d :: Int -> [Int]
funcQ14d value = funcQ14d_2 value periodo

{--Questão 15 ------------------------------------------------------} 
{-
    @param: Int
    @explain: Consultaremos a sequência de Fibonacci de tamanho dinâmico, onde se o valor for igual ao parâmetro retorna sua casa na sequência.
    Enquanto não for igual, segue "contando casas". Caso não encontre, retorna -1.
    @demonstrate: antFib 13 = fibonacci 0 0 = fibonacci 1 1 fibonacci 2 2 ... fibonacci 13 13.
    @return: Posição de n na sequência de Fibonacci.
    @OBSERVAÇÃO : Usamos Int para tornar a consulta o mais abrangente possível pois Int's não permitem números grandes de acordo com seu tamanho máximo.

-}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

funcQ15 :: Integer -> Integer -> Integer
funcQ15 x cont
    |x > fibonacci cont = funcQ15 x (cont+1)
    |x == fibonacci cont = cont
    |otherwise = -1

antFib :: Integer -> Integer
antFib x = funcQ15 x 0

{--Questão 16 ------------------------------------------------------} 
{-
    @OBSERVAÇÃO : Podemos substituir facilmente a segunda guarda por um "ou" lógico, afinal se x > y > z, x > todos, sendo True.
-}
funny :: Int -> Int -> Int -> Bool
funny x y z = (x > z) || (y < x)

{--Questão 17 ------------------------------------------------------} 
{-
    @param: Char
    @explain: Tradicionalmente, pela função ord, podemos verificar que a diferença entre um caractere minúsculo e um maiúsculo está em 32. 
    Verique, por exemplo a função ord 'C' e, depois, ord 'c'. P/ C, temos 67 e p/ c, 99.
    @demonstrate: funcQ17 'a' = chr((97-32)) = chr(65) = 'A'.
    @return: Caractere maiúsculo.

-}
funcQ17 :: Char -> Char
funcQ17 crt
    | (crt == 'ç') = 'Ç'
    | (crt == 'Ç') = 'Ç'
    | (isLower crt) = chr((ord crt) - 32)
    | otherwise = crt

{--Questão 18 ------------------------------------------------------} 
{-
    @param: Char
    @explain: Tradicionalmente, pela função ord, podemos verificar que a diferença entre um caractere númerico e sua ordem está em 48. Verique, por exemplo
    a função ord '3' e, veja que seu resultado soma 48 ao digito digitado. Então, semelhantemente à função funcQ17, usamos a mesma lógica com a função 'ord' da biblioteca Data.Char
    @demonstrate: charToNum '1' = 49-48 = 1. Outro exemplo, '9' = 57-48 = 9.
    @return: Número de 1 algarismo convertido para Int.

-}
charToNum :: Char -> Int
charToNum crt
    | (isDigit (crt) == True) = ((ord crt) - 48)
    | otherwise = (-1)

{--Questão 19 ------------------------------------------------------} 
{-
    @param: String, Int
    @explain: Concatenaremos a própria string em si mesma 'n' vezes, resultando em n+1 cópias (pois contamos com a original). Para retirar a original, só
    decrementar até 1.
    @demonstrate: duplicate "oi" 3 = oi++oi++oi++oi.
    @return: String concatenada consigo mesma n+1 vezes.

-}
duplicate :: String -> Int -> String
duplicate s n
    | (n > 0) = s++duplicate s (n-1)
    | otherwise = s

{--Questão 20 ------------------------------------------------------} 
{-
    @param: String, Int
    @explain: Se uma string for de tamanho 5 e desejamos torna-la de tamanho 7, devemos concatenar ">"2 vezes em seu início.
    @demonstrate: pushRight "oi" 3 = ">"++pushRight "oi" 2 = ">oi".
    @return: String de tamanho x+n, onde x é seu tamanho e n o parâmetro inteiro. 

-}
pushRight :: String -> Int -> String
pushRight s n
    | (n > length (s)) = ">"++pushRight s (n-1)
    | otherwise = s

{--Questão 21 ------------------------------------------------------} 

--Item21.a.a
infixl 6 &-
(&-):: Int->Int->Int
x &- y = x - 2*y
{-
    Como este operador é associativo à esquerda, sua operação começa da esquerda
    para a direita, seguindo a ordem (x, y) estabelecida no exercício. 
    O resultado dessa operação é
    (10) - 2*(3) = 4
    (4) - 2*(2) = 0, resultado final.

-}
--Item21.a.b
infixr 6 &--
(&--):: Int->Int->Int
x &-- y = x - 2*y
{-
    Como este operador é associativo à esquerda, sua operação começa da direita
    para a esquerda, seguindo a ordem (x, y) estabelecida no exercício. 
    (3) - 2*(2) = -1
    (10) - 2*(-1) = 10 + 2 = 12
-}
--Item21.a.c
infix 6 &---
(&---):: Int->Int->Int
x &--- y = x - 2*y
{-
    Como este operador é não associativo, não há como definir em uma linha múltiplas 
    utilizações do mesmo. A associatividade existe para inserir ordem de execução
    em expressões. A título de exemplo, os operadores (<, >, <=, >=) são não
    associativos. Eles não podem comparar, por exemplo, 3 < 2 < 1.
-}

--Item21.b.a
{-
    Quando atribuímos 8 ou 7 ao operador, estamos atribuíndo ordem de precedência.
    Dessa forma, para a operação INFIXA, sem associatividade, teremos:

    infix -> (10) - 2*(6) = -2, perceba que neste exemplo, a multiplicação
    teve precedência.

    Para precedência 8, houve maior importância o operador criado.
    Então, a multiplicação só foi realizada por último.

    infix -> (10) - 2*(3) = 4 * 2 = 8 

-}
infix 8 &-&
(&-&):: Int->Int->Int
x &-& y = x - 2*y

{--Questão 22 ------------------------------------------------------} 
{-
    @param: [Int]
    @explain: Inverter uma lista, resumidamente, é concatenar sua cauda (que também é uma lista) com a cabeça até se tornar vazia.
    @demonstrate: [1, 2] = inverte [2]++(1).
    @return: Lista invertida.

-}
inverte :: [Int] -> [Int]
inverte [] = []
inverte (cabeca:cauda) = inverte (cauda)++[cabeca]

{--Questão 23 ------------------------------------------------------} 
{-
    @param: [Int]
    @explain: Converter um inteiro em lestra basta somar seu correspondente ASCII em 64, tornando-o um caractere do alfabeto. 
    Após isso, basta concatená-lo. Continua até ser lista vazia, recebendo String "".
    @demonstrate: converte [1, 2] = chr(65):chr(66) = "AB".
    @return: String correspondente a lista de inteiros.

-}
funcQ23 :: Int -> Char
funcQ23 n = chr (n + 64)

converte :: [Int] -> String
converte [] = ""
converte (cabeca:cauda) = funcQ23 (cabeca):converte (cauda)

{--Questão 24 ------------------------------------------------------} 

--Item24.a
{-
    ['a'..'g']
    Essa expressão vai criar uma string contendo as letras de a a g, sendo:
    "abcdefg"
-}
--Item24.b
{-
    [0.1..0.9]
    Essa expressão irá criar uma lista de ponto flutuante que será incrementada em 1, produzindo:
    [0.1..1.1]
    Nota: Essa incrementação só ocorre pois não foi denotada maior precisão, como [0.1, 0.2..0.9]
-}
--Item24.c
{-
    [0.1, 0.3..0.9]
    Essa expressão irá criar números ímpares até o 0.9, criando uma lista de ponto flutuante com os mesmos.
    [0.1, 0.3, 0.5, 0.7, 0.9]    
-}
--Item24.d
{-
    [0.1,0.3..1.8]
    Essa expressão irá criar números ímpares até o 1.8, criando uma lista de ponto flutuante com os mesmos.
    Porém, como 1.8 é par, seu ímpar vizinho superior será acrescentado em seu lugar.
    [0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9]  
-}
--Item24.e
{-
    [0.4,0.2..0.8]
    Essa expressão não possui lógica definida em uma PA, logo é impossível denotar uma lista
    que contenha seus integrantes, resultando em uma lista sem tipo [] 
-}
--Item24.f
{-
    [1.4,..15]
    Essa expressão irá incrementar em 1 unidade até o 15, criando uma lista de ponto flutuante terminada em 0.4.
    [1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 7.4, 8.4, 9.4, 10.4, 11.4, 12.4, 13.4, 14.4, 15.4]  
    O "sobressalente" 15.4 existe pois dá sentido à lista, criando situação de 'overflow' sob seu término 15. 
    Sem esse ítem, a lista seria incompleta.
-}

{--Questão 25 ------------------------------------------------------} 
{-
    @param: [Char], Char
    @explain: Dada uma lista de cabeca:cauda,  sempre que sua cabeça for igual ao char desejado, devemos incrementar o contador. Caso não,
    percorre a lista usando a cauda.
    Dessa maneira, ao término da execução teremos o número de ocorrências na lista de Char.
    @demonstrate: conta ['a', 'b', 'a'] 'a' = ['a'] == 'a', cont+1, ['b'], cont, ['a'] = 'a', cont+1 = 2.
    @return: Número de ocorrências de um char em uma lista de Char..

-}
funcQ25 :: [Char] -> Char -> Int -> Int
funcQ25 [] _ n = n
funcQ25 (cabeca:cauda) c n
    | (cabeca == c) = funcQ25 cauda c (n+1)
    | (cabeca /= c) = funcQ25 cauda c n
    | otherwise = n

conta :: [Char] -> Char -> Int
conta (cabeca:cauda) c = funcQ25 (cabeca:cauda) c 0

{--Questão 26 ------------------------------------------------------} 
{-
    @param: [Int]
    @explain: Sempre que o item da cabeça for elemento repetido, o excluímos não concatenando-o com uma lista auxiliar. Caso contrário, concatenamos a cabeça
    com tal lista.
    @return: Lista ordenada sem repetições.
    @OBSERVAÇÃO: Elem é uma função que verifica se 'x' é elemento de uma determinada Lista.
-}
funcQ26 :: [Int] -> [Int] -> [Int]
funcQ26 [] aux = inverte (aux)
funcQ26 (cabeca:cauda) aux
    | ((elem) cabeca aux) = funcQ26 cauda aux
    | not ((elem) cabeca aux) = funcQ26 cauda (cabeca:aux)

purifica :: [Int] -> [Int]
purifica lista = funcQ26 lista []

{--Questão 27 ------------------------------------------------------} 
{-
    @param: [Int]
    @explain: Para a cauda, tornaremos a sua cauda a função que prolifera de caordo com o valor da cabeça.
    O valor da cabeça se torna proliferador sempre que chamada a função funcQ27.
    @return: Lista com valores proliferados de acordo com elementos da Lista.

-}
funcQ27 :: Int -> Int -> [Int]
funcQ27 _ 0 = []
funcQ27 cauda proliferador = cauda : funcQ27 cauda (proliferador-1)


proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (cabeca:cauda) = (funcQ27 cabeca cabeca) ++ proliferaInt cauda

{--Questão 28 ------------------------------------------------------} 
{-
    @param: [Char]
    @explain: Mesma lógica da função proliferaInt, porém, deveremos agorda tratar os char's com a função ord, decrementando 64 para manter
    o padrão, retornando um Int e concantenando a cabeça com a cauda.
    @return: Lista com valores proliferados de acordo com elementos da Lista.
    
-}
funcQ28 :: Char -> Int -> String
funcQ28 _ 0 = ""
funcQ28 cauda proliferador = cauda : funcQ28 cauda (proliferador-1)

proliferaChar :: [Char] -> String
proliferaChar [] = ""
proliferaChar (cabeca:cauda) = funcQ28 cabeca (ord cabeca - 64)++proliferaChar cauda