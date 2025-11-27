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

maquinaDeus :: Int -> Int
maquinaDeus jogador =
    case jogador of
        1 -> 2
        2 -> 3
        3 -> 1
        4 -> 5
        5 -> 1
        _ -> 1

maquinaTonto :: Int -> Int
maquinaTonto jogador =
    case jogador of
        1 -> 3
        2 -> 1
        3 -> 2
        4 -> 1
        5 -> 4
        _ -> 1

rodarJogo :: (Int -> IO Int) -> IO ()
rodarJogo maquinaEscolha = do
    putStrLn "Escolha uma opção: (1) Pedra, (2) Papel, (3) Tesoura, (4) Fogo, (5) Água, (0) Sair"
    input <- getLine
    let numero = read input :: Int
    if numero == 0
        then putStrLn "Obrigado por jogar!"
        else do
            putStrLn $ "Você escolheu " ++ jogada numero ++ "."
            maquina <- maquinaEscolha numero
            putStrLn $ "A máquina escolheu " ++ jogada maquina ++ "."
            putStrLn $ vencedor numero maquina
            rodarJogo maquinaEscolha

modoNormal :: Int -> IO Int
modoNormal _ = escolhaMaquina

modoDeus :: Int -> IO Int
modoDeus n = return (maquinaDeus n)

modoTonto :: Int -> IO Int
modoTonto n = return (maquinaTonto n)

main :: IO ()
main = do
    putStrLn "=== Pedra, Papel, Tesoura, Fogo e Água ==="
    putStrLn "Escolha o modo de jogo:"
    putStrLn "1 - Normal"
    putStrLn "2 - Modo Deus"
    putStrLn "3 - Modo Tonto"
    modo <- getLine
    case modo of
        "1" -> rodarJogo modoNormal
        "2" -> rodarJogo modoDeus
        "3" -> rodarJogo modoTonto
        _   -> putStrLn "Modo inválido!"
