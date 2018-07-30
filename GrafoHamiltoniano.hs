module Main where

import Grafo
import Data.List.Split

-- Definição de Grafo Hamiltoniano:
-- Um grafo hamiltoniano é grafo que possui um caminho que permite passar por todos os vértices de um grafo G, 
-- não repetindo nenhum vertice e formando um ciclo.

-- Teorema 1: (Teorema de Ore) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que a soma dos graus de cada par de vértices não-adjacentes seja no mínimo n.
teoremaUm grafo = Grafo.teoremaDeOre grafo

-- Teorema 2: (Teorema de Dirac) Uma condição suficiente (mas não necessária) para que um grafo G 
-- seja hamiltoniano é que o grau de todo vértice de G seja no mínimo n/2, onde n é o número de vértices em G.
teoremaDois grafo = Grafo.teoremaDeDirac grafo

-- Percorre um grafo em busca de um caminho hamiltoniano.
buscarCaminhoHamiltoniano grafo = Grafo.percorrerGrafo grafo

-- Teorema 1 e 2 são chamados primeiramente, caso nenhum dos dois seja satisfeito, 
-- é realizada a busca utilizando 'força bruta'.
isHamiltoniano grafo =  if (teoremaUm grafo || teoremaDois grafo) then True else buscarCaminhoHamiltoniano grafo

-- Método auxiliar para o I/O
convertToGrafo [] = []
convertToGrafo (x:xs) = [( read (head (splitOn "," x)) :: Integer, read (head (tail (splitOn "," x))) :: Integer )] ++ convertToGrafo xs 
-- Método auxiliar para o I/O
boolToString True = "O grafo inserido eh Hamiltoniano"
boolToString False = "O grafo inserido nao eh Hamiltoniano"

-- Main
main :: IO()
main = do
    putStrLn "Insira um grafo para checar se ele é Hamiltoniano ou não"
    putStrLn "(Exemplo de entrada: 1,2 2,3 3,1)"
    let input = ""
    input <- getLine

    let grafo = convertToGrafo (splitOn " " input);

    let result = isHamiltoniano grafo
    let output = boolToString result

    print output