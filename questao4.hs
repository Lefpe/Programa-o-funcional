import Data.List (gcd)

-- Função para calcular o resto da divisão de dois números inteiros positivos
restoDivisao :: Int -> Int -> Int
restoDivisao dividendo divisor
  | divisor == 0 = error "Divisão por zero"
  | otherwise = dividendo `mod` divisor

-- Função para calcular a divisão inteira entre dois números inteiros positivos
divisaoInteira :: Int -> Int -> Int
divisaoInteira dividendo divisor
  | divisor == 0 = error "Divisão por zero"
  | otherwise = dividendo `div` divisor

-- Função para calcular o máximo divisor comum entre dois números inteiros positivos
mdc :: Int -> Int -> Int
mdc = gcd

-- Função para calcular o mínimo múltiplo comum entre dois números inteiros positivos
mmc :: Int -> Int -> Int
mmc a b = (a * b) `div` mdc a b

-- Função principal que interage com o usuário
main :: IO ()
main = do
  putStrLn "cálculo de divisão, MDC e MMC!"
  putStrLn "Por favor, digite o dividendo (o número que será dividido):"
  dividendoStr <- getLine
  putStrLn "Agora, digite o divisor (o número pelo qual o dividendo será dividido):"
  divisorStr <- getLine
  let dividendo = read dividendoStr :: Int
      divisor = read divisorStr :: Int
  putStrLn $ "\nResultados:"
  putStrLn $ "O resto da divisão de " ++ show dividendo ++ " por " ++ show divisor ++ " é: " ++ show (restoDivisao dividendo divisor)
  putStrLn $ "A divisão inteira de " ++ show dividendo ++ " por " ++ show divisor ++ " é: " ++ show (divisaoInteira dividendo divisor)
  putStrLn $ "O MDC de " ++ show dividendo ++ " e " ++ show divisor ++ " é: " ++ show (mdc dividendo divisor)
  putStrLn $ "O MMC de " ++ show dividendo ++ " e " ++ show divisor ++ " é: " ++ show (mmc dividendo divisor)

