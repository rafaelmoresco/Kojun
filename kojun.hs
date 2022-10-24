module Main where

import Reader
import Matrix
import Data.List

type Choices = [Value]

-- Solucao baseada na de Graham Hutton. 
-- Disponível em: http://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs

-- Recebe a matriz de valores e de posições lida do documento de texto.
-- Retorna a primeira solução encontrada para o tabuleiro.
solve :: Grid -> Grid -> Grid
solve vals pos = (search (prune (choices vals pos) pos) pos)!!0

-- Recebe uma matriz de valores e a matriz de posições.
-- Retorna uma matriz de escolhas.
-- Preenche cada celula sem valor com uma lista contendo valores possíveis para aquela célula.
-- Essa lista vai de 1 até o tamanho do bloco correspondente a célula, excluindo valores pré-existentes no bloco.
choices :: Grid -> Grid -> Matrix Choices
choices vals pos = map (map choice) (zipWith zip vals pos)
    where choice (v, p) = if v == 0 then [1..(lengthOfBlock p pos)] `minus` (valsOfBlock vals pos p) else [v]

-- Recebe uma matriz de escolhas e a matriz de posições.
-- Aplica a função reduce para cada coluna dividida por blocos.
-- Retorna essa matriz de escolhas com escolhas reduzidas.
prune :: Matrix Choices -> Grid -> Matrix Choices
prune vals pos = cols $ colsOfBlocksByCols (map reduce (blocksByCols vals pos)) (size vals)

-- De uma linha contendo escolhas, reduz as escolhas com base em elementos unitários
-- ex: ["1 2 3 4", "1", "3 4", "3"] -> ["2 4", "1", "4", "3"]
reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
    where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

-- Retorna true se a lista passada só possui um elemento
single :: [a] -> Bool
single [_] = True
single _ = False

-- Recebe uma matriz de escolhas e a matriz de posições.
-- Retorna uma lista que contém soluções válidas para o tabuleiro.
-- A ideia desse algoritmo é de que filtre todas as escolhas possíveis, uma célula por vez,
-- e retorne somente matrizes que contém escolhas válidas.
search :: Matrix Choices -> Grid -> [Grid]
search vals pos
    -- nao retorna nada se a matriz de escolhas passada não pode fornecer uma solução
    | blocked vals pos = []
    -- se a matriz de escolhas passada é válida e só contém valores unitários, é uma solução,
    -- portanto, extrai o valor de cada lista de escolhas e retorna.
    | all (all single) vals = [map concat vals]
    -- se a matriz de escolhas passada é valida e contém mais de uma escolha para pelo menos uma célula,
    -- expande a matriz, reduz o número de escolhas restantes e continua o processo de busca sobre ela.
    | otherwise = [g | vals' <- expand vals, g <- search (prune vals' pos) pos]

-- Recebe uma matriz de escolhas e a matriz de posições.
-- Retorna true se a matriz de escolhas passada nunca pode fornecer uma solução.
blocked :: Matrix Choices -> Grid -> Bool
blocked vals pos = void vals || not (safe vals pos)

-- Verifica se ha alguma celula vazia na matriz
void :: Matrix Choices -> Bool
void m = any (any null) m

-- Recebe uma matriz de escolhas e a matriz de posições.
-- Retorna true se as matrizes passam nos seguintes testes:
-- -- Cada célula não possui vizinhos com o mesmo valor;
-- -- Cada bloco não possui valores duplicados;
-- -- Cada bloco respeita a ordem de valores decrescentes na vertical.
safe :: Matrix Choices -> Grid -> Bool
safe vals pos = all (validNeighborhood) (cols vals) &&
        all (validNeighborhood) (rows vals) &&
        all (nodups) (blocks vals pos) &&
        all (isDecreasing) (blocksByCols vals pos)

-- Verifica se nao ha valores unitarios vizinhos iguais
validNeighborhood :: Eq a => Row [a] -> Bool
validNeighborhood [] = True
validNeighborhood [a] = True
validNeighborhood (a:b:bs) 
    | (length a <= 1) && (length b <= 1) = if a == b then False else validNeighborhood (b:bs)
    | otherwise = validNeighborhood (b:bs)

-- Verifica se não há valores unitarios duplicados na linha passada
nodups :: Eq a => Row [a] -> Bool
nodups [] = True
nodups (x:xs) = if (length x <= 1) then not (elem x xs) && nodups xs else nodups xs

-- Verifica se os valores unitários da linha passada estão em ordem decrescente
isDecreasing :: Ord a => Row [a] -> Bool
isDecreasing [] = True
isDecreasing [a] = True
isDecreasing (a:b:bs) 
    | (length a <= 1) && (length b <= 1) = if a < b then False else isDecreasing (b:bs)
    | otherwise = isDecreasing (b:bs)

-- Expand funciona de modo similar à collapse. A diferença é que faz o collapse
-- apenas para a primeira célula que contém mais de uma escolha.
expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1,row:rows2) = break (any (not . single)) m
        (row1,cs:row2) = break (not . single) row

main = do
    putStrLn "Insira o path do tabuleiro (ex.: Tabuleiros/tabuleiro10x10.txt):"
    board <- getLine
    (vals, pos) <- readPuzzle board
    let solutions = solve vals pos
    putStrLn "Solucao:"
    mapM_ print solutions