module Grafo where

type Vertice = Integer
type Aresta = (Vertice, Vertice)
type Grafo = [Aresta]


-- Representação de um grafo desconexo
grafoDesconexo :: [Aresta]
grafoDesconexo = [(1,2), (1,3), (1,4), (1,5),
		(2,6), (2,7), (4,8), (5,9)]

-- Representação de um grafo conexo
grafoConexo :: [Aresta]	
grafoConexo = [(1,2),(2,3),(3,1)]

-- Representação de mais um grafo
grafoTeste = [(1,2), (2,3), (3,4), (4,5), (5,1)]

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
	then False else conexo aux grafo

isGrauPar grafo = grauPar grafo grafo

-- Verifica se o grau de todos os vértices de um grafo é no mínimo n/2, onde n = numero de vértices do grafo.

dirac :: Grafo -> Grafo -> Int -> Bool
dirac [] grafo verticesLength = True
dirac (a:aux) grafo verticesLength = if fromIntegral (length (adjacentes grafo (fst a))) >= (fromIntegral verticesLength / 2.0) &&
 fromIntegral (length (adjacentes grafo (snd a))) >= (fromIntegral verticesLength / 2.0) then dirac aux grafo verticesLength else False

teoremaDeDirac grafo = dirac grafo grafo (length (getVertices grafo []))