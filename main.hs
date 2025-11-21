import System.Random

jogada :: Int -> String
jogada n = case n of
    1 -> "pedra"
    2 -> "papel"
    3 -> "tesoura"
    4 -> "fogo"
    5 -> "água"
    _ -> "jogada inválida"

escolhaMaquina :: IO Int
escolhaMaquina = randomRIO (1, 5)

vencedor :: Int -> Int -> String
vencedor jogador maquina
    | jogador == maquina = "Empate!"
    | 
      (jogador == 1 && maquina == 3) ||
      (jogador == 3 && maquina == 2) ||
      (jogador == 2 && maquina == 1) ||
      (jogador == 4 && (maquina == 1 || maquina == 2 || maquina == 3)) ||
      ((jogador == 1 || jogador == 2 || jogador == 3) && maquina == 5) ||
      (jogador == 5 && maquina == 4)
        = "Você venceu!"
    | otherwise = "A máquina venceu!"

main :: IO ()
main = do
    putStrLn "Escolha uma opção: (1) Pedra, (2) Papel, (3) Tesoura, (4) Fogo, (5) Água"
    input <- getLine
    let numero = read input :: Int
    putStrLn $ "Você escolheu " ++ jogada numero ++ "."
    
    maquina <- escolhaMaquina
    putStrLn $ "A máquina escolheu " ++ jogada maquina ++ "."
    
    putStrLn $ vencedor numero maquina
