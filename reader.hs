-- Modulo responsavel por ler e mapear o documento tabuleiro.txt
module Reader where

import Matrix

-- A partir da String passada, cria uma lista divindo-a onde a condição passada for verdadeira
-- exemplo: splitWhen (==",") "Minha,string" -> retorna ["Minha", "string"]
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen condition str = case dropWhile condition str of
                    "" -> []
                    str' -> splitted: splitWhen condition str''
                        where (splitted, str'') = break condition str'

-- Lê o arquivo indicado em FilePath
-- Retorna duas matrizes:
-- -- A primeira matriz (chamamos de matriz de valores) contém o valor da célula ou 0;
-- -- A segunda matriz (chamamos de matriz de posições) contém o número do bloco associado à célula.
readPuzzle :: FilePath -> IO (Grid, Grid)
readPuzzle p = do
    puz <- readFile p
    return $ lineProcessing (lines puz)

lineProcessing :: [[Char]] -> (Grid, Grid)
lineProcessing xs = ([splitElements (words x) 0 | x <- xs], [splitElements (words x) 1 | x <- xs])

-- ex: splitElements [["A,B", "C,D"], ["E,F", "G,H"]] 0 -> [[A, C], [E, G]]
splitElements :: [String] -> Int -> [Int]
splitElements [] _ = []
splitElements (element : list) pos = [value] ++ splitElements list pos
    where value = read ((splitWhen (==',') element)!!pos) :: Int