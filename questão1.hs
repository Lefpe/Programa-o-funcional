potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base expoente = base * potencia base (expoente - 1)

main :: IO ()
main = do
  putStrLn "Digite a base:"
  baseStr <- getLine
  putStrLn "Digite o expoente:"
  expoenteStr <- getLine
  let base = read baseStr :: Int
      expoente = read expoenteStr :: Int
  putStrLn $ "O resultado da potência é: " ++ show (potencia base expoente)
