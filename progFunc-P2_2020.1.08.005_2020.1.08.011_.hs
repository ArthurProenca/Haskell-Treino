{------------------------------------------ 
Prova 02 Programação Funcinonal 18/08/2021
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
    @explain: Explicação da função.
-}

{-FUNÇÕES ÚTEIS EM MÚLTIPLOS CONTEXTOS-}
separaItem :: [a] -> a 
separaItem (cabeca:cauda) = cabeca

auxTamanho :: [a] -> Int -> Int
auxTamanho [] tam = tam
auxTamanho (h:t) tam = auxTamanho t (tam + 1)

tamanho :: [a] -> Int
tamanho [] = 0
tamanho l = auxTamanho l 0

sumElem :: [Integer] -> Integer -> Integer
sumElem [] cont = cont
sumElem (h:t) cont = sumElem t (cont + h)

somaElemLista :: [Integer] -> Integer
somaElemLista [] = 0
somaElemLista l = sumElem l 0

{--Questão 1 ----------------------------------------------}

type Brinde = (String, Int, Int)

tabela1 :: Int -> Brinde
tabela1 01 = ("Natal", 21, 34)
tabela1 02 = ("Bertioga", 17, 65)
tabela1 03 = ("Rio de Janeiro", 9, 10)
tabela1 04 = ("Curitiba", 3, 54)
tabela1 05 = ("Petrolina", 2, 09)
tabela1 06 = ("Salvador", 0, 01)
tabela1 07 = ("Teresina", 21, 56)

--Item 1.a
{-
    @explain: Função simples, consulta a tabela1 e retorna uma tupla correspondente.
    Adicionamos tratamento de usuário p/ números maiores que a tabela.
-}
mapa :: Int -> Brinde
mapa x 
    | x >= 1 && x < 8 = tabela1 x
    | otherwise = ("", 0, 0)

--Item 1.b.cidade
{-
    @explain: Consulta a tupla, retornando apenas o nome. Caso contrário, exibe mensagem de erro.
-}
funcQ1b1 :: Int -> Brinde -> String
funcQ1b1 0 _ = "Tupla nao consta na tabela."
funcQ1b1 index (city, npassagens, nhospedagens)
    | mapa index == (city, npassagens, nhospedagens) = city
    | otherwise = funcQ1b1 (index - 1) (city, npassagens, nhospedagens)

cidade :: Brinde -> String
cidade x = funcQ1b1 7 x

--Item1.b.nPassagens
{-
    @explain: Consulta a tupla, retornando apenas o número de passagens.
    Adicionamos o modifier para usarmos somente esta função
        0 -> Passagens
        1 -> Hospedagens 
-}
funcQ1b2 :: Int -> Brinde -> Int -> Int
funcQ1b2 0 _ _ = 0
funcQ1b2 index (city, npassagens, nhospedagens) modifier
    | mapa index == (city, npassagens, nhospedagens) && modifier == 0 = npassagens
    | mapa index == (city, npassagens, nhospedagens) && modifier == 1 = nhospedagens
    | mapa index /= (city, npassagens, nhospedagens) && modifier == 1 = funcQ1b2 (index - 1) (city, npassagens, nhospedagens) 1
    | otherwise = funcQ1b2 (index - 1) (city, npassagens, nhospedagens) 0

nPassagens :: Brinde -> Int
nPassagens x = funcQ1b2 7 x 0

--Item1.b.nHospedagens
nHospedagens :: Brinde -> Int
nHospedagens x = funcQ1b2 7 x 1

--Item 1.c
{-
    @explain: Usa um acumulador, retornando-o após diversas consultas às funções anteriores.
-}
auxPassagens :: Int -> Int
auxPassagens 0 = 0
auxPassagens x = nPassagens (mapa x) + auxPassagens (x - 1) 

funcQ1c :: Int
funcQ1c = auxPassagens 7

--Item 1.d
{-
    @explain: Usa um acumulador, retornando-o após diversas consultas às funções anteriores.
-}
auxHospedagens :: Int -> Int
auxHospedagens 0 = 0
auxHospedagens x = nHospedagens (mapa x) + auxHospedagens (x - 1) 

funcQ1d :: Int
funcQ1d = auxHospedagens 7

--Item 1.e
{-
    @explain: Basicamente, consulta a tabela e armazena, ainda usando as funções anteriores.
-}
auxQe :: String -> Int -> (Int, Int, Int)
auxQe _ 0 = (0, 0, 0)
auxQe city cont
    | cidade (mapa cont) == city = (cont, ((auxPassagens cont) - auxPassagens (cont - 1)), ((auxHospedagens cont) - auxHospedagens (cont - 1)))
    | otherwise = auxQe city (cont - 1)

funcQ1e :: String -> (Int, Int, Int)
funcQ1e city = auxQe city 7 

--Item 1.f
auxQf :: Brinde -> Int -> Bool
auxQf _ 0 = False
auxQf (city, passagens, hospedagem) cont
    | city == cidade (mapa cont) && passagens <= nPassagens (mapa cont) && hospedagem <= nHospedagens (mapa cont) = True
    | otherwise = auxQf (city, passagens, hospedagem) (cont - 1)

funcQ1f :: Brinde -> Bool
funcQ1f tupla = auxQf tupla 7

{--Questão 2 ----------------------------------------------}

{--
--Item2.a
        Dada uma lista de (Char, Int), essa função irá comparar os valores contidos na
    parte inteira da lista, onde o menor inteiro será impresso junto de seu Char ao final.
    Caso contrário, irá pegar o item de índice menor e continuar a execução recursivamente.

    Imagine que foi inserida a lista [('a', 2), ('b', 1), ('c', 3)]
    *Como 1 é menor que 2, a lista continuará em [('b', 1), ('c', 3)]
    *Como 1 é menor que 3, a lista continuará em [('b', 1)]
    *Como a lista de um item, o padrão (Char, Int) casa em [t] = t, retornando ('b', 1)
--}
{--
--Item2.b
        Dados uma dupla (Char, Int) e uma lista de [(Char, Int)], essa função irá comparar os
    índices inteiros e, caso forem iguais, remove o item da lista e para a execução. Caso contrário
    irá continuar a execução para o mesmo (Char, Int) na cauda da lista [(Char, Int)]

    Imagine que foi inserido ('b', 1) [('a', 1), ('b', 1), ('c', 3)]
    *Como 1 é igual a 1, o item ('a', 1) é removido da lista e a execução retorna [('b', 1),('c', 3)]
    *Perceba que mesmo que exista ('b', 1) na lista, a remoção está baseada no índice inteiro

--}
{--
--Item2.c
        Dada uma lista de [(Char, Int)], a função f01 irá criar uma string com: o primeiro elemento menor
    da lista, junto do restante excluíndo o mesmo. Por exemplo:
    
    Imagine que foi inserido [('a', 1), ('b', 1), ('c', 3)]
    *Como 1 é igual a 1, a função continua em [('b', 1), ('c', 3)]
    *Como 1 é menor que 3, a função então recebe [('b', 1)]
    *Até então temos: 'b':f01 [('b', 1), ('c', 3)]
    *Como 1 é menor que 3, a função retorna ('b', 1)
    *Então, temos: 'b':'b':f01 [('c', 3)]     nota: Já incluí a exclusão de ('b', 1)
    *Como é o único elemento, a função retorna ('c', 3)
    *Então, temos: 'b':'b':'c':f01 []
    *Como a Lista vazia é base, a execução para com uma lista de char, ou seja, uma string: "bbc" = ['b', 'b', 'c']
--}
{--
--Item2.d
        A função proximo é chamada recursivamente e externamente pela função f01. Ela não precisa
    de base com lista vazia pois a assinatura da função na sua essência é lista de duplas. Portanto,
    ao receber uma lista [] não há casamento de padrão. Além disso, a função f01 já trata a lista vazia.

    Também, ao percorrer uma lista e chegar em seu último elemento, o tipo genérico [t] garante o retorno
    de t, tirando a necessidade de casar padrão com lista vazia no momento de parada para retorno.
--}
{--
--Item2.e
        Em proximo, os tipos de t, a, b, c, e x são:

        t -> Tipo genérico, pode ser qualquer outra forma de lista.
        a -> Tipo Char
        b -> Tipo Int
        c -> Tipo Char
        d -> Tipo Int
        x -> Tipo Lista.
--}

{--Questão 3 ----------------------------------------------}
{-
    @explain: Basicamente, usamos as operações de chr e demais comparações.
    Por mais que tenha dado muitas funções, tentamos tratar os erros mais comuns.
-}

converteLista :: [Int] -> String
converteLista [] = []
converteLista (cabeca:cauda) = chr (cabeca+97):converteLista cauda

auxQ3 :: [Int] -> [Int] -> String -> String -> Int -> String
auxQ3 [] _ _ _ _ = []
auxQ3 (c:t) lAux (c1:t1) aux cont
    | chr (c+97) == c1 = converteLista lAux
    | chr (c+97) /= c1 && cont < tamanho (c1:t1) = auxQ3 (c:t) lAux t1 aux (cont+1)
    | chr (c+97) /= c1 && cont == tamanho (c1:t1) = auxQ3 t lAux aux aux 0

funcQ3 :: [[Int]] -> String -> [String]
funcQ3 [] _ = []
funcQ3 (cabeca:cauda) s 
    | [[a | a <- auxQ3 cabeca cabeca s s 0]] == [""] = funcQ3 cauda s
    | otherwise = [[a | a <- auxQ3 cabeca cabeca s s 0]] ++ funcQ3 cauda s

{--Questão 4 ----------------------------------------------}
{-
    @explain: Como no enunciado diz "a mesma dada", no singular, tratamos apenas uma string na lista de strings.
-}
funcQ4 :: [String] -> [(Int, String)]
funcQ4 x = [(tamanho y, b) | y <- x, b <- x]

{--Questão 5 ----------------------------------------------}
{-
    @explain: Perceba que usamos map na função chamada (funcQ5).
    map, função de alta ordem, pega a função auxQ5 junto da lista digitada pelo usuário.
-}
tiraImpar :: [Int] -> Int -> [Int]
tiraImpar [] _ = []
tiraImpar (h:t) cont
    | (mod) cont 2 == 0 = h:tiraImpar t (cont + 1)
    | otherwise = tiraImpar t (cont + 1)

auxQ5 :: ([Int], Bool) -> [Int]
auxQ5 ([], _) = []
auxQ5 (l, b)
    | b == True = l
    | b == False = tiraImpar l 0

funcQ5 :: [([Int], Bool)] -> [[Int]]
funcQ5 [([], _)] = [[]]
funcQ5 l = map auxQ5 l

{--Questão 6 ----------------------------------------------}
{-
    Compara item a item, com seu anterior e cria um sistema de verificação.
    Ignoramos apenas o primeiro item, colocando-o sempre (como consta na linha 304)
-}
par :: Int -> Bool
par a
    | (mod) a 2 == 0 = True
    | otherwise = False

auxQ6 :: [Int] -> Int -> [Int]
auxQ6 [] _ = []
auxQ6 (h:t) ant
    | not(par ant) && par h = auxQ6 t h
    | par(ant) = h:auxQ6 t h
    | not(par ant) && not(par h) = h:auxQ6 t h

funcQ6 :: [Int] -> [Int]
funcQ6 [] = []
funcQ6 (h:t) = h:auxQ6 t h

{--Questão 7 ----------------------------------------------}
{-
    @explain: Compara item a item, concatenando o contador com a próxima chamada.
    @ATENÇÃO: funcQ7 é uma função de alta ordem, defve ter em sua chamada a função (funcQ6)
-}
auxQ7 :: [Int] -> Int -> Int -> [Int]
auxQ7 [] _ _ = []
auxQ7 (h:t) ant cont
    | not(par ant) && par h = cont:auxQ7 t h (cont + 1)
    | par(ant) = auxQ7 t h (cont + 1)
    | not(par ant) && not(par h) = auxQ7 t h (cont + 1)

--O segundo parâmetro deve ser a chamada funcQ6, obedecendo a avaliação.
funcQ7 :: [Int] -> ([Int] -> [Int]) -> ([Int], [Int])
funcQ7 [] _ = ([], [])
funcQ7 l f
    | l == f l = (l, [])
    | l /= f l = (f l, auxQ7 l (separaItem l) 0)

{--Questão 8 ----------------------------------------------}
type Empresa = [(String, Bool, [Integer])]
--1 ano = 52 semanas
empresa = [("0101", True, [1..52]), ("1010", False, ([0..25]++(map (*3) [0..27]))), ("1111", True, [1, 11..52]), ("0000", False, ([0..25]++(map (*0) [0..27])))]


ocorrenciaZero :: Integer -> Integer -> [Integer] -> Integer
ocorrenciaZero _ cont [] = cont
ocorrenciaZero x cont (h:t)
    | x == h = ocorrenciaZero x (cont + 1) t 
    | x /= h = ocorrenciaZero x cont t

--Item 8.a
auxQ8a :: Empresa -> Int -> [String]
auxQ8a [] _ = []
auxQ8a  ((s, b, l):t) modifier
    | b == True && modifier == 0 = s:auxQ8a t modifier
    | b == False && modifier == 1 = s:auxQ8a t modifier
    | b /= True && modifier == 0 = auxQ8a t modifier
    | b /= False && modifier == 1 = auxQ8a t modifier

--Filiais
funcQ8af :: [String]
funcQ8af = auxQ8a empresa 0

--Candidatas
funcQ8ac :: [String]
funcQ8ac = auxQ8a empresa 1

--Item 8.b
auxQ8b :: String -> Empresa -> Integer
auxQ8b _ [] = 0
auxQ8b nome ((s, b, l):t)
    | nome == s = somaElemLista l
    | nome /= s = auxQ8b nome t

verificaMeta :: Integer -> Bool
verificaMeta res
    | res >= 1200 = True
    | otherwise = False

verificaZeros :: String -> Empresa -> Bool
verificaZeros _ [] = False
verificaZeros n ((s, b, l):t)
    | s == n && ocorrenciaZero 0 0 l < 6 = True
    | otherwise = verificaZeros n t

funcQ8b :: String -> Bool
funcQ8b "" = False
funcQ8b nome = verificaMeta (auxQ8b nome empresa) && verificaZeros nome empresa

--Item 8.c
auxQ8c :: Empresa -> [(String, Bool)]
auxQ8c [] = []
auxQ8c ((s, b, l):t) = (s, funcQ8b s):auxQ8c t

funcQ8c :: [(String, Bool)]
funcQ8c = auxQ8c empresa 

--Item 8.d
auxQ8d :: Empresa -> [String]
auxQ8d [] = []
auxQ8d ((s, b, l):t)
    | b == False && verificaMeta (auxQ8b s empresa) && verificaZeros s empresa = s:auxQ8d t
    | otherwise = auxQ8d t

funcQ8d :: [String]
funcQ8d = auxQ8d empresa

--Item 8.e
type Gerente = String
type NovaEstrutura = [(String, Bool, [Int], Gerente)]

{--Questão 9 ----------------------------------------------}
{-
    @explain: Usamos um acumulador que conta se o item da cabeça é um número.
    Se é número, aumenta. Se não, continua a execução até lista vazia.
-}
auxQ9 :: String -> Int -> Int
auxQ9 [] cont = cont
auxQ9 (h:t) cont
    | ord h > 57 = auxQ9 t cont
    | ord h >=48 && ord h <= 57 = auxQ9 t (cont + 1)

funcQ9 :: String -> Int
funcQ9 "" = 0
funcQ9 a = auxQ9 a 0

{--Questão 10 ----------------------------------------------}
{-
    @explain: Usamos a ordem de casamento de padrão para evitar índice negativo.
    A função usa da sua auxiliar para determinar o índice e ítem.
    Se o índice for igual à cabeça retorna a posição e se a posição for igual ao índice, True.
-}
type L = [Int]

auxQ10 :: L -> Int -> Int -> Int
auxQ10 [] _ _ = -1 --Para evitar que o primeiro índice seja verificado.
auxQ10 (h:t) x cont
    | h == x = cont
    | h /= x = auxQ10 t x (cont + 1)

funcQ10 :: L -> Int -> Bool
funcQ10 [] _ = False
funcQ10 l x
    | x < 0 = False
    | x == (auxQ10 l x 0) = True
    | otherwise = False

{--Questão 11 ----------------------------------------------}
auxQ11 :: (Int, Int) -> Int -> Bool
auxQ11 (x,y) z
    | x + y > z = True
    | otherwise = False

funcQ11 :: [(Int, Int)] -> Int -> [Bool]
funcQ11 [] _ = [False]
funcQ11 l x = [auxQ11 a x | a <- l]

{--Questão 12 ----------------------------------------------}
{-
    @explain: Essa função retorna uma dupla de listas, usamos o conceito de listas comprimidas ([1..3] = [1,2,3])
-}
auxQ12a :: Int -> [Int] -> [Int]
auxQ12a _ [] = []
auxQ12a a (h:t)
    | (mod) h a == 0 = h:auxQ12a a t
    | (mod) h a /= 0 = auxQ12a a t

auxQ12b :: Int -> [Int] -> [Int]
auxQ12b _ [] = []
auxQ12b a (h:t)
    | (mod) h a == 0 = auxQ12b a t
    | (mod) h a /= 0 = h:auxQ12b a t

funcQ12 :: Int -> Int -> ([Int], [Int])
funcQ12 x y = ((auxQ12a x [x..y]), (auxQ12b x [x..y]))

{--Questão 13 ----------------------------------------------}
auxQ13 :: String -> Int -> String
auxQ13 _ 0 = ""
auxQ13 s cont = s++auxQ13 s (cont - 1)

funcQ13 :: String -> String
funcQ13 "" = ""
funcQ13 s = auxQ13 s (tamanho s)

{--Questão 14 ----------------------------------------------}

--Item 14.a
{-
    Uma função de alta ordem é uma função que permite receber uma
    função como parâmetro, com assinatura geralmente definida no cabeçalho.
    Uma função de alta ordem comum é a função map, por exemplo:
        map (^2) [1..10] resultará em [1,4,9,16,25,36,49,64,81,100]
    Perceba que (^2) é uma função e foi passada como parâmetro.
-}

--Item 14.b
{-
    A vantagem em utilizar funções de alta ordem consiste na possibilidade
    de reaproveitamento de código e encapsulamento de parâmetro, onde por 
    exemplo, um programador pode produzir uma função em haskell que recebe outra
    função como parâmetro sem se preocupar com a confecção desta função parâmetro.
-}

--Item 14.c
{-
    Uma avaliação preguiçosa é uma forma de operar a semântica em Haskell,
    usando o conceito de que só é processado um resultado quando é necessário.
    Ou seja, é como se o Haskell fosse procrastinador.
    Basicamente,

    fst ("Preguica", [1..10])

    Conseguiriamos o resultado "Preguica", afinal não foi necessário "chamar" a lista.

    Essa é a abordagem  "topo-outros", onde o topo é processado sem necessidade do
    processamento dos outros, quando necessário.

-}
{--Questão 15 ----------------------------------------------}
type Id = Integer
type Nome = String
type Telefone = String

type Pessoa = [(Id, Nome, Telefone)]
type Conhece = [(Id, [Id])] --Pessoa x conhece lista de pessoas.

pessoas = [(0, "Arthur", "11111"), (1, "Eliseu", "22222"), (2, "Pagliares", "33333"), (3, "Mariane", "44444"), (4, "Fulano", "00000"), (5, "John Lennon", "23456")]
conhece = [(0, [1..3]), (1, [0,2]), (2, [1,3]), (3, [0,1,2]), (4, []), (5, [])]

auxNI :: (Integer, String) -> String -> Integer
auxNI (i, s) n
    | s == n = i
    | otherwise = -1

nomeID :: (Id, Nome, Telefone) -> Pessoa -> String -> Id
nomeID _ _ "" = -1
nomeID (id, nome, _) [] n = auxNI (id, nome) n
nomeID (id, nome, tel) (h:t) n 
    | (nome == n) = id
    | (nome /= n) = nomeID h t n

conheceID :: Integer -> Conhece -> [Integer]
conheceID _ [] = []
conheceID id (h:t)
    | id == (fst h) = snd h 
    | id /= (fst h) = conheceID id t

--Lista de pessoas q conhece, comparando na lista.
naoConhece :: [Integer] -> Conhece -> [Integer]
naoConhece [] (h:t) = [(fst h)..fst (separaItem (reverse t))]
naoConhece (h:t) (h1:t1)
    | h == (fst h1) = naoConhece t t1
    | h /= (fst h1) = (fst h1):naoConhece (h:t) t1

--Dado ID, pego a lista de nomes
auxIN :: (Integer, String) -> Integer -> String
auxIN (i, s) i2
    | i == i2 = s
    | otherwise = ""

idNome :: (Id, Nome, Telefone) -> Pessoa -> Integer -> String
idNome (id, nome, _) [] n = auxIN (id, nome) n
idNome (id, nome, telefone) (h:t) x
    | (id == x) = nome
    | (id /= x) = idNome h t x

--Lista de ids que o nome conhece.
listaConhece :: String -> [Integer]
listaConhece nome = (conheceID (nomeID (separaItem pessoas) pessoas nome) conhece)

--Item 15.a
funcQ15a :: String -> Int
funcQ15a nome = tamanho (conheceID (nomeID (separaItem pessoas) pessoas nome) conhece)

--Item 15.b
funcQ15b :: String -> [String]
funcQ15b nome = [idNome (separaItem pessoas) pessoas a | a <- (listaConhece nome)]

--Item 15.c
auxNT :: (String, String) -> String -> String
auxNT (i, s) n
    | i == n = s
    | otherwise = ""

nomeTelefone :: (Id, Nome, Telefone) -> Pessoa -> String -> Telefone
nomeTelefone _ _ "" = ""
nomeTelefone (id, nome, tel) [] n = auxNT (nome, tel) n
nomeTelefone (id, nome, tel) (h:t) n 
    | (nome == n) = tel
    | (nome /= n) = nomeTelefone h t n

funcQ15c :: String -> String
funcQ15c "" = ""
funcQ15c nome = nomeTelefone (separaItem pessoas) pessoas nome

--Item 15.d
funcQ15d :: String -> [String]
funcQ15d "" = []
funcQ15d nome = [idNome (separaItem pessoas) pessoas a | a <- naoConhece (listaConhece nome) conhece ]

--Item 15.e
auxQ15e :: Conhece -> [Integer]
auxQ15e [] = []
auxQ15e (h:t)
    | (snd h) == [] = (fst h):auxQ15e t
    | (snd h) /= [] = auxQ15e t

funcQ15e :: [String]
funcQ15e = [idNome (separaItem pessoas) pessoas a | a <- auxQ15e conhece]

{--Questão 16 ----------------------------------------------}
{-
    @explain: Dada uma lista de duplas, retornaremos o índice do primeiro item da dupla.
-}
auxQ16 :: (Int, String) -> Int -> Char
auxQ16 (index, (h:t)) cont
    | cont == index = h
    | cont /= index = auxQ16 (index, t) (cont + 1)

funcQ16 :: [(Int, String)] -> String
funcQ16 [] = ""
funcQ16 l = [auxQ16 a 0| a <- l]

{--Questão 17 ----------------------------------------------}
{-
    @explain: Ficamos confusos, mas tentamos implementar algo que seja válido. Fomos até o
    limite de representação dos Int's em Haskell em computadores de 32bit.
-}
funcQ17 :: [Int]
funcQ17 = [0..2147483647] --Vamos até o limite de Int em Haskell para 32bit.

{--Questão 18 ----------------------------------------------}
{-
    @explain: Função de alta ordem, onde a função exclui elementos em True.
    Usando list comprehension, definios que "x é cada elemento da lista" e a nova lista só entra quando True.
-}
filtraElimina :: [Int] -> (Int -> Bool) -> [Bool]
filtraElimina [] _ = []
filtraElimina l f = [f x | x <- l, ((f x) == True)]

{--Questão 19 ----------------------------------------------}
{-
    @explain: dado que é uma dupla estática, criamos 2 funções para cada item da tupla.
    Dessa forma, fica mais simples a implementação.
    Usamos o conceito de modifier para aproveitamento da função auxQ19a, onde
        modifier = 2 é par
        modifier = 3 é impar
-}
auxQ19a :: [Int] -> Int -> [Int]
auxQ19a [] _ = []
auxQ19a (h:t) modifier
    | (mod) h modifier == 0 = h:auxQ19a t modifier
    | (mod) h modifier /= 0 = auxQ19a t modifier

auxQ19b :: [Int] -> [Int]
auxQ19b [] = []
auxQ19b (h:t)
    | (mod) h 2 /= 0 && (mod) h 3 /= 0 = h:auxQ19b t
    | otherwise = auxQ19b t

funcQ19 :: [Int] -> ([Int], [Int], [Int])
funcQ19 [] = ([], [], [])
funcQ19 l = (auxQ19a l 2, auxQ19a l 3, auxQ19b l)

{--Questão 20 ----------------------------------------------}
{-
    @explain: Basicamente, compara cada item de cada sublista, buscando o maior.
    Quando a sublista acaba, retorna o maior. 
-}
auxQ20 :: [Int] -> Int -> Int
auxQ20 [] temp = temp
auxQ20 (h:t) temp
    | t /= [] && h > (separaItem t) && temp >= h = auxQ20 t temp
    | t /= [] && h < (separaItem t) && temp >= (separaItem t) = auxQ20 t temp
    | t /= [] && h < (separaItem t) && temp < (separaItem t) = auxQ20 t (separaItem t)
    | t /= [] && h == (separaItem t) = auxQ20 t h
    | t == [] = temp

funcQ20 :: [[Int]] -> [Int]
funcQ20 [[]] = []
funcQ20 ll = [auxQ20 a (separaItem a) | a <- ll]

{--Questão 21 ----------------------------------------------}
{-
    @explain: Se o char é item da lista, concatena True com a próxima chamada.
    Até que a lista seja vazia.
-}
testa :: Char -> [Char] -> [Bool]
testa _ [] = []
testa c (cabeca:cauda)
    | c == cabeca = True:testa c cauda
    | otherwise = False:testa c cauda

{--Questão 22 ----------------------------------------------}
geraListaDupla :: [Int] -> Int -> [(Int, Int)]
geraListaDupla l i = [(a, b) | a <- l, b <- [i], a > i]

{--Questão 23 ----------------------------------------------}
{-
    @explain: buscamos funções genêricas, com definições em tempo de compilação.
    Logo, independente da lista, será separado.
    Usamos a função purifica da avaliação 1 para contribuição. 
-}
inverte [] = []
inverte (cabeca:cauda) = inverte (cauda)++[cabeca]

purifica [] aux = inverte (aux)
purifica (cabeca:cauda) aux
    | ((elem) cabeca aux) = purifica cauda aux
    | not ((elem) cabeca aux) = purifica cauda (cabeca:aux)
--Fim de funções auxiliares.
auxQ23 _ [] = False
auxQ23 aux (h:t)
    | aux == h = True
    | aux /= h = auxQ23 aux t

repetidos [] = []
repetidos (h:t)
    | auxQ23 h t = purifica(h:repetidos t) []
    | otherwise = purifica(repetidos t) []

{--Questão 24 ----------------------------------------------}
{-
    @explain: As listas comprimidas permitem definir uma espécie de Progressão Aritmética.
    Logo, apenas coordenamos e colocanos no list comprehension.
-}
termina :: Int -> [Int]
termina x = [a | a <- [x, x+10..100]]

{--Questão 25 ----------------------------------------------}
--Item 25.a
distancia :: Int -> Int -> Int
distancia x y = (x^2 + (x-y)^2)

auxQ25a :: Int -> Int -> Int -> Int
auxQ25a x k1 k2
    | (distancia x k1) > (distancia x k2) = k2
    | (distancia x k1) < (distancia x k2) = k1
    | otherwise = k1

infix 7 &&&
(&&&) :: Int -> (Int, Int)-> Int
x &&& k = auxQ25a x (fst k) (snd k)

--Item 25.b
--Aplicar a função n vezes, colocando o resultado na lista.
funcQ25b :: (Int->(Int,Int)->Int) -> Int -> [(Int, Int)] -> [Int]
funcQ25b _ _ [] = []
funcQ25b f x (h:t) = (f x h):funcQ25b f x t

--Item 25.c
{-
 Chamaremos a função por meio de: 
    funcQ25b (&&&) 3 [(3,10), (1,11), (2, 3)]
 A primeira execução trará:
 f = &&&
 x = 3
 [(3,10), (1,11), (2, 3)]
 Com execução ((&&&) 3 (3,10)) = 3:funcQ25b (&&&) 3 [(1,11), (2, 3)]

 A segunda execução trará:
 f = &&&
 x = 3
 [(1,11), (2, 3)]
 Com execução ((&&&) 3 (1,11)) = 3:1:funcQ25b (&&&) 3 [(2, 3)]

 A terceira execução trará:
 f = &&&
 x = 3
 [(2, 3)]
 Com execução ((&&&) 3 (2,3)) = 3:1:3

 A quarta execução trará:
 f = &&&
 x = 3
 []
 Casará padrão com a lista vazia e retornará [], dando 3:1:3:[]

 Por fim, será retornado [3,1,3]
-}

{--Questão 26 ----------------------------------------------}
{-
    @explain: usamos novamente a função purifica. Perceba que 
    ela recebe o resultado da list comprehension.
-}
auxQ26 :: Char -> [Char] -> Int -> (Char, Int)
auxQ26 c [] cont = (c, cont)
auxQ26 c (h:t) cont
    | c == h = auxQ26 c t (cont + 1)
    | c /= h = auxQ26 c t cont

mescla :: [Char] -> [Char] -> [(Char, Int)]
mescla l1 carbon = purifica ([auxQ26 a b 0 | a <- l1, b <- [carbon]]) []

{--Questão 27 ----------------------------------------------}
{-
    @explain: usando uma árvore de chamadas às funções maior/menor, podemos definir a dupla final.
-}
maior :: Int -> Int -> Int
maior x y
    | x > y = x
    | x < y = y

menor :: Int -> Int -> Int
menor x y
    | x < y = x
    | x > y = y

infix 3 -*-
(-*-) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x, y) -*- (v, w) = (maior(maior x y) (maior v w), menor(menor x y) (menor v w))

{--Questão 28 ----------------------------------------------}
--Item 28.a
{-
    @explain: Novamente, para duplas estáticas podemos trabalhar muito facilmente,
    usando apenas a função necessária no contexto.
-}
auxQ28 :: Int -> Int -> [Int] -> Int
auxQ28 _ cont [] = cont
auxQ28 x cont (h:t)
    | x == h = auxQ28 x (cont + 1) t
    | x /= h = auxQ28 x cont t

ocorrencia :: Int -> L -> (Int, Int)
ocorrencia x [] = (x, 0)
ocorrencia x l = (x, auxQ28 x 0 l)

--Item 28.b
{-
    @explain: entendemos que serão n duplas para o tamanho da lista onde
    (item, numero de ocorrências)
    Exemplo:
    aplica ocorrencia [1..10] = [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)]

-}
auxQ28b :: (Int->[Int]->(Int,Int)) -> [Int] -> [Int] -> [(Int, Int)]
auxQ28b _ [] _ = []
auxQ28b f (h:t) l = (f h l):auxQ28b f t l

aplica :: (Int->[Int]->(Int,Int)) -> [Int] -> [(Int, Int)]
aplica _ [] = []
aplica f l = auxQ28b f l l

--Item 28.c
{-
 Podemos utilizar ocorrência com a seguinda chamada:
    aplica ocorrencia [1, 1, 2, 2]
-}

{--Questão 29 ----------------------------------------------}
funny x y z = (x > z) || ((y >= x) && True) 

{--Questão 30 ----------------------------------------------}
infix 3 -.-
(-.-) :: Int -> [Int] -> [Int]
x -.- [] = []
x -.- (h:t)
    | x == h = t
    | x /= h = h:(x -.- t)
