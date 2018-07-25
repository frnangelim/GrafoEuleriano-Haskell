module Main where
-- Teorema 1: (Teorema de Ore) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que a soma dos graus de cada par de vértices não-adjacentes seja no mínimo n.

-- Teorema 2: (Teorema de Dirac) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que o grau de todo vértice de G seja no mínimo n/2, onde n é o número de vértices em G.

import Grafo
import Data.List.Split

teoremaUm grafo = Grafo.teoremaDeOre grafo

teoremaDois grafo = Grafo.teoremaDeDirac grafo

percorrerGrafo grafo = Grafo.caminharNoGrafo grafo

isHamiltoniano grafo =  if (teoremaUm grafo || teoremaDois grafo) then True else percorrerGrafo grafo


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