module Main where
--definir potencia 
potencia :: Int -> Int -> Int
potencia_0 = 1
potencia base expoente = base * potencia base (expoente - 1)
--estrutura principal 
main :: IO ()
main = do
--erro aqui
putStrLn "Digite a base:"
baseStr <- getLine 
putStrLn "Digite o expoente:"
expoente <- getLine
let resultado = potencia base expoente
if expoente < 0 
then putStrLn "O expoente deve ser não negativo."
else do
putStrLn ("O resultado de " ++ show base ++ " elevado a " ++ show resultado 

