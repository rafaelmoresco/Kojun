-- Módulo responsável por gerenciar matrizes
module Matrix where

import Data.List

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

-- Se a matriz passada é quadrada, retorna sua dimensão
size :: Matrix a -> Int
size m = length (m!!0)

-- Retorna as linhas de uma matriz
rows :: Matrix a -> [Row a]
rows m = m

-- Retorna as colunas da matriz passada (transposicao)
cols :: Matrix a -> [Row a]
cols m = transpose m

-- Divide a matriz de valores passada de acordo com a matriz de posições.
-- Ou seja, retorna uma matriz de blocos.
blocks :: Eq a => Matrix a -> Grid -> Matrix a
blocks vals pos = [filterByGroup group tupleValueGroup | group <- groups]
  where
    tupleValueGroup = foldl1 (++) (zipWith zip vals pos)
    groups = nub (map snd tupleValueGroup)
    filterByGroup group list = map fst $ filter ((== group) . snd) list

-- Recebe uma matriz de valores, a de posições e o número de um bloco.
-- Retorna uma lista composta pelos valores pertencentes àquele bloco.
valsOfBlock :: Eq a => Matrix a -> Grid -> Int -> [a]
valsOfBlock vals pos id = map fst $ filter ((==id) . snd) tupleValueGroup 
  where
    tupleValueGroup = foldl1 (++) (zipWith zip vals pos)

-- Recebe uma matriz de valores e a de posiçoes.
-- Retorna as colunas da matriz de valores divididas por blocos.
-- Ex: [[1, 2, 3], [4, 5, 6]], [[0, 0, 2], [1, 0, 2]] -> [[1,2], [3], [4], [5], [6]]
blocksByCols :: Eq a => Matrix a -> Grid -> [Row a]
blocksByCols vals pos = zipWith zip (cols vals) (cols pos) >>= map (map fst) . groupBy (\a b -> snd a == snd b)

-- Recebe o número de um bloco e a matriz de posições.
-- Retorna o tamanho do bloco correspondente.
lengthOfBlock :: Eq a => a -> Matrix a -> Int
lengthOfBlock _ [] = 0
lengthOfBlock id pos = sum [count id p | p <- pos]
  where count x xs = length (filter (==x) xs)

-- Recebe uma lista de colunas divididas por blocos e o tamanho do tabuleiro.
-- A partir da lista de colunas dividas por blocos, remonta as colunas originais
-- da matriz de valores
colsOfBlocksByCols :: [Row a] -> Int -> [Row a]
colsOfBlocksByCols bs n = chunksOf n (concat bs)

-- Divide a lista passada em sublistas de tamanho n
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)