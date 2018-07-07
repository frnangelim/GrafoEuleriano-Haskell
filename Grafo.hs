module Grafo where

type Vertice = Int
type Aresta = (Vertice, Vertice)
type Grafo = [Aresta]


-- Representação de um grafo desconexo
grafo :: [Aresta]
grafo = [
		(1,2), (1,3), (1,4), (1,5),
		(2,6), (2,7), (4,8), (5,9)
	]

-- Representação de um grafo conexo
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