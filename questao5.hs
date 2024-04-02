-- Função para verificar se um número é perfeito
ehPerfeito :: Int -> Bool
ehPerfeito n = n == somaDivisores n 1
  where
    somaDivisores :: Int -> Int -> Int
    somaDivisores num divisor
      | divisor == num = 0  -- Retorna 0 quando atingimos o próprio número, para evitar incluí-lo na soma
      | num `mod` divisor == 0 = divisor + somaDivisores num (divisor + 1)  -- Adiciona o divisor atual e continua recursivamente
      | otherwise = somaDivisores num (divisor + 1)  -- Continua recursivamente sem adicionar o divisor atual

-- Função principal que interage com o usuário
main :: IO ()
main = do
  putStrLn "verificador de números perfeitos!"
  putStrLn "digite um número natural maior que 0:"
  numStr <- getLine
  let num = read numStr :: Int
  if ehPerfeito num
    then putStrLn $ "o número" ++ show num ++ " é um número perfeito."
    else putStrLn $ "O número " ++ show num ++ " não é um número perfeito."
