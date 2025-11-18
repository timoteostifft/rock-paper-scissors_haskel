jogada :: Int -> String
jogada n = case n of
    1 -> "pedra"
    2 -> "papel"
    3 -> "tesoura"
    _ -> "jogada inválida"

main :: IO ()
main = do
    putStrLn "Escolha uma opção: (1) Pedra, (2) Papel, (3) Tesoura"
    input <- getLine
    let numero = read input :: Int
    putStrLn $ "Você escolheu " ++ jogada numero ++ "."
