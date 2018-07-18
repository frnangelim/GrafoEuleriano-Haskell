module Main where
-- Teorema 1: Um grafo conexo G é um grafo hamiltoniano se e somente se todo vértice de G possui grau par. 

-- Teorema 2: Um grafo conexo G é um grafo hamiltoniano se e somente se ele pode ser decomposto em circuitos. 

-- Teorema 3: (Teorema de Ore) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que a soma dos graus de cada par de vértices não-adjacentes seja no mínimo n.

-- Teorema 4: (Teorema de Dirac) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que o grau de todo vértice de G seja no mínimo n/2, onde n é o número de vértices em G.

import Grafo

teoremaZero = Grafo.isConexo -- Se não for conexo, não é hamiltoniano(verificar*)

teoremaUm grafo = Grafo.isConexo grafo && Grafo.isGrauPar grafo

teoremaTres = Grafo.nadjacentes (Grafo.adjacentes Grafo.grafoTeste2 1) (Grafo.getVertices Grafo.grafoTeste2 [])

teoremaQuatro grafo = Grafo.teoremaDeDirac grafo

main :: IO()
main = do
	putStrLn "Insira um grafo para checar se ele é Euleriano"
	let input = ""
	input <- getLine
	putStrLn input
