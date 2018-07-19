module Main where
-- Teorema 1: Um grafo conexo G é um grafo hamiltoniano se e somente se todo vértice de G possui grau par. 

-- Teorema 2: Um grafo conexo G é um grafo hamiltoniano se e somente se ele pode ser decomposto em circuitos. 

-- Teorema 3: (Teorema de Ore) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que a soma dos graus de cada par de vértices não-adjacentes seja no mínimo n.

-- Teorema 4: (Teorema de Dirac) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que o grau de todo vértice de G seja no mínimo n/2, onde n é o número de vértices em G.

import Grafo
import Data.List.Split


teoremaZero grafo = Grafo.isConexo grafo -- Se não for conexo, não é hamiltoniano(verificar*)

teoremaTres grafo = Grafo.teoremaDeOre grafo

teoremaQuatro grafo = Grafo.teoremaDeDirac grafo

isHamiltoniano grafo = teoremaZero grafo && (teoremaTres grafo || teoremaQuatro grafo);

boolToString True = "O grafo inserido eh hamiltoniano"
boolToString False = "O grafo inserido nao eh hamiltoniano"

convertToGrafo [] = []
convertToGrafo (x:xs) = [( read (head (splitOn "," x)) :: Integer, read (head (tail (splitOn "," x))) :: Integer )] ++ convertToGrafo xs 

main :: IO()
main = do
	putStrLn "Insira um grafo para checar se ele é Euleriano"
	let input = ""
	input <- getLine

	let grafo = convertToGrafo (splitOn " " input);

	let result = isHamiltoniano grafo
	let output = boolToString result

	print output