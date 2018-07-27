module Grafo where

type Vertice = Integer
type Aresta = (Vertice, Vertice)
type Grafo = [Aresta]


-- Representações de grafos, utilizados para testes.
grafoTeste :: [Aresta]
grafoTeste = [(1,2), (2,3), (3,4), (4,5), (5,1)]
grafoTeste2 :: [Aresta]
grafoTeste2 = [(1,3), (1,2), (2,3), (3,4), (3,5), (5,4)]
grafoTeste3 :: [Aresta]
grafoTeste3 = [(1,4),(1,2),(1,5),(5,3),(5,4),(4,3),(3,2)]
grafoDesconexo :: [Aresta]
grafoDesconexo = [(1,2), (1,3), (1,4), (1,5),(2,6), (2,7), (4,8), (5,9)]
grafoConexo :: [Aresta]
grafoConexo = [(1,2),(2,3),(3,1)]

-- Obtem uma lista com todos vértices adjacentes a um dado vértice
adjacentes :: Grafo -> Vertice -> [Vertice]
adjacentes [] _ = []
adjacentes ((a,b):c) v
        | (a == v) = b:(adjacentes c v)
        | (b == v) = a:(adjacentes c v)
        | otherwise = adjacentes c v

-- Verifica se um elemento já existe em uma lista de tuplas
-- Utilizado no metodo getVertices
contains :: [Vertice] -> Integer -> Bool 
contains [] elem = False
contains (x:xs) elem = if x == elem then True else contains xs elem

-- Obtém todos os vertices de um grafo
getVertices :: Grafo -> [Vertice] -> [Vertice]
getVertices [] vertices = vertices
getVertices (a:as) vertices = if not (contains vertices (fst a)) then getVertices (a:as) ((fst a):vertices) 
    else if not (contains vertices (snd a)) then getVertices as ((snd a):vertices) else getVertices as vertices

-- Teorema de Dirac: 
-- verifica se o grau de todos os vértices de um grafo é no mínimo n/2, onde n = numero de vértices do grafo.
dirac :: Grafo -> Grafo -> Int -> Bool
dirac [] grafo verticesLength = True
dirac (a:aux) grafo verticesLength = if fromIntegral (length (adjacentes grafo (fst a))) >= (fromIntegral verticesLength / 2.0) &&
 fromIntegral (length (adjacentes grafo (snd a))) >= (fromIntegral verticesLength / 2.0) then dirac aux grafo verticesLength else False
teoremaDeDirac :: Grafo -> Bool
teoremaDeDirac grafo = dirac grafo grafo (length (getVertices grafo []))


-- Retorna o grau de um vertice em um determinado grafo.
sumGraus :: Grafo -> Vertice -> Int
sumGraus [] vertice = 0
sumGraus (x:xs) vertice = if(fst(x) == vertice) then 1 + sumGraus xs vertice else if(snd(x) == vertice) then 1 + sumGraus xs vertice else sumGraus xs vertice 

-- Remove um vértice de um lista de vértices.
remove :: Vertice -> [Vertice] -> [Vertice]
remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)

-- Remove de uma lista de vertices Y, todos vértices de X.
-- Onde Y = todos os vértices de um grafo,
-- e X = vértices adjacentes ao vertice atual.
nadjacentes :: [Vertice] -> [Vertice] -> [Vertice]
nadjacentes [] vertices = vertices
nadjacentes (x:xs) (y:ys) = nadjacentes xs (remove x (y:ys))

-- Retorna os vertices não adjacentes a um vertice em um determinado grafo.
getNadjacentes :: Grafo -> Vertice -> [Vertice]
getNadjacentes grafo vertice = nadjacentes (adjacentes grafo vertice) (remove vertice (getVertices grafo []))

-- Verifica para um vertice, se a soma do seu grau com o grau de cada vertice adjacente a ele,
-- é no minimo n, onde n = numero de vértices do grafo.
sumGrausVertices :: Grafo -> Vertice -> [Vertice] -> Bool
sumGrausVertices grafo vertice [] = True
sumGrausVertices grafo vertice (v:vertices) = if (sumGraus grafo v + sumGraus grafo vertice) >= length (getVertices grafo []) then sumGrausVertices grafo vertice vertices else False

-- Teorema de Ore:  a soma dos graus de cada par de vértices não-adjacentes deve ser no mínimo n, 
-- onde n = numero de vértices do grafo.
ore :: Grafo -> Grafo -> Bool
ore grafo [] = True
ore grafo (x:xs) = if (sumGrausVertices grafo (fst x) (getNadjacentes grafo (fst x))) && (sumGrausVertices grafo (snd x) (getNadjacentes grafo (snd x))) then ore grafo xs else False
teoremaDeOre :: Grafo -> Bool
teoremaDeOre grafo = ore grafo grafo

-- Compara os vertices percorridos com todos os vertices do grafo
compararAux :: [Vertice] -> [Vertice] -> Bool
compararAux vertices [] = True
compararAux vertices (x:xs) = if (contains vertices x) then compararAux vertices xs else False
compararVertices :: Grafo -> [Vertice] -> Bool
compararVertices grafo verticesPercorridos = 
    let grafoVertices = (getVertices grafo []) in (compararAux grafoVertices verticesPercorridos) 
    && (length grafoVertices == length verticesPercorridos)

-- Retorna apenas os vértices que não foram percorridos
removerPercorridos :: [Vertice] -> [Vertice] -> [Vertice]
removerPercorridos [] percorridos = []
removerPercorridos (x:xs) percorridos = if (contains percorridos x) 
    then removerPercorridos xs percorridos else [x] ++ removerPercorridos xs percorridos

-- Retorna quais vertices adjacentes ao vertice atual, ainda não foram percorridos
verticesDisponiveis :: Grafo -> Vertice -> [Vertice] -> [Vertice]
verticesDisponiveis grafo verticeAtual verticesCaminhados = 
    removerPercorridos (adjacentes grafo verticeAtual) verticesCaminhados

-- Divide os caminhos a serem percorridos.
dividirCaminho :: Grafo -> [Vertice] -> [Vertice] -> Bool
dividirCaminho grafo verticesCaminhados (x:[]) = percorrer grafo verticesCaminhados x
dividirCaminho grafo verticesCaminhados (x:xs) = 
    percorrer grafo verticesCaminhados x || dividirCaminho grafo verticesCaminhados xs

-- Percorre um grafo em busca de um caminho hamiltoniano, se encontrar, retorna True.
percorrer :: Grafo -> [Vertice] -> Vertice -> Bool
percorrer grafo verticesCaminhados verticeAtual = let disponiveis = verticesDisponiveis grafo verticeAtual verticesCaminhados in
    if (disponiveis) /= []
    then dividirCaminho grafo (verticesCaminhados ++ [verticeAtual]) disponiveis 
    else (compararVertices grafo (verticesCaminhados ++ [verticeAtual])) && (head (adjacentes grafo verticeAtual) == (fst (head grafo)))

-- Realizada a chama para percorrer um grafo em busca de um caminho hamiltoniano.
percorrerGrafo :: Grafo -> Bool
percorrerGrafo grafo = percorrer grafo [] (fst (head grafo))







------------------ Metodos que passaram a não ser utilizados ---------------------

-- Verifica se um grafo é conexo
-- Um grafo será conexo se todos seus vértices possuirem 2 ou mais vértices adjacentes
conexo :: Grafo -> Grafo -> Bool
conexo [] grafo = True
conexo (a:aux) grafo = if not ( length (adjacentes grafo (fst a)) > 1) then False else if not ( length (adjacentes grafo (snd a)) > 1) 
    then False else conexo aux grafo

isConexo grafo = conexo grafo grafo

-- Verifica se todos vértices de um grafo possui grau par
grauPar :: Grafo -> Grafo -> Bool
grauPar [] grafo = True
grauPar (a:aux) grafo = if not ( length (adjacentes grafo (fst a)) `mod` 2 == 0) then False else if not ( length (adjacentes grafo (snd a)) `mod` 2 == 0) 
    then False else grauPar aux grafo

isGrauPar grafo = grauPar grafo grafo

------------------ Metodos que passaram a não ser utilizados ---------------------
