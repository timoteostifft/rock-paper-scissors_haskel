import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Tela
    = Menu
    | Jogo String
    | Resultado String String String
    deriving (Eq, Show)

data Estado = Estado
    { tela :: Tela }

type Botao = (Float, Float, Float, Float, String)

botoesMenu :: [Botao]
botoesMenu =
    [ (-150, 50, 300, 60, "Normal")
    , (-150, -50, 300, 60, "Deus")
    , (-150, -150, 300, 60, "Tonto")
    ]

botoesJogada :: [Botao]
botoesJogada =
    [ (-300, 100, 200, 60, "Pedra")
    , (-300, 0,   200, 60, "Papel")
    , (-300, -100,200, 60, "Tesoura")
    , (100,  50,  200, 60, "Fogo")
    , (100, -50,  200, 60, "Água")
    ]

main :: IO ()
main = do
    play
        (InWindow "Pedra Papel Tesoura Fogo Água" (800, 600) (50, 50))
        white
        30
        (Estado Menu)
        desenhar
        evento
        atualizar

desenhar :: Estado -> Picture
desenhar (Estado Menu) =
    Pictures (Texto "Escolha o modo:" 0 200 : map desenharBotao botoesMenu)

desenhar (Estado (Jogo modo)) =
    Pictures (Texto ("Modo: " ++ modo ++ " | Selecione sua jogada:") 0 200 : map desenharBotao botoesJogada)

desenhar (Estado (Resultado j m r)) =
    Pictures
    [ Texto ("Você: " ++ j) 0 100
    , Texto ("Máquina: " ++ m) 0 0
    , Texto ("Resultado: " ++ r) 0 (-100)
    , Texto "Clique para voltar" 0 (-200)
    ]

desenhar _ = Blank

Texto :: String -> Float -> Float -> Picture
Texto str x y =
    Translate x y $
    Scale 0.2 0.2 $
    Color black $
    Text str

desenharBotao :: Botao -> Picture
desenharBotao (x, y, w, h, nome) =
    Pictures
        [ Translate x y $ Color (greyN 0.8) $ rectangleSolid w h
        , Translate x y $ Scale 0.15 0.15 $ Color black $ Text nome
        ]

clicou :: Float -> Float -> Botao -> Bool
clicou mx my (x, y, w, h, _) =
    abs (mx - x) <= w/2 && abs (my - y) <= h/2

evento :: Event -> Estado -> Estado
evento (EventKey (MouseButton LeftButton) Up _ (mx, my)) est@(Estado Menu) =
    case lookupBotao mx my botoesMenu of
        Just "Normal" -> Estado (Jogo "normal")
        Just "Deus"   -> Estado (Jogo "deus")
        Just "Tonto"  -> Estado (Jogo "tonto")
        _             -> est

evento (EventKey (MouseButton LeftButton) Up _ (mx, my)) est@(Estado (Jogo modo)) =
    case lookupBotao mx my botoesJogada of
        Just jogadaStr ->
            let jogador = jogadaParaNum jogadaStr
                maq = jogadaMaquina modo jogador
            in Estado (Resultado jogadaStr (numParaJogada maq) (vencedor jogador maq))
        _ -> est

evento (EventKey (MouseButton LeftButton) Up _ _) (Estado (Resultado _ _ _)) =
    Estado Menu

evento _ e = e

atualizar :: Float -> Estado -> Estado
atualizar _ e = e

lookupBotao :: Float -> Float -> [Botao] -> Maybe String
lookupBotao mx my = foldr (\b acc -> if clicou mx my b then Just (nome b) else acc) Nothing
  where nome (_,_,_,_,n) = n

jogadaParaNum :: String -> Int
jogadaParaNum "Pedra" = 1
jogadaParaNum "Papel" = 2
jogadaParaNum "Tesoura" = 3
jogadaParaNum "Fogo" = 4
jogadaParaNum "Água" = 5
jogadaParaNum _ = 1

numParaJogada :: Int -> String
numParaJogada 1 = "Pedra"
numParaJogada 2 = "Papel"
numParaJogada 3 = "Tesoura"
numParaJogada 4 = "Fogo"
numParaJogada 5 = "Água"
numParaJogada _ = "?"

jogadaMaquina :: String -> Int -> Int
jogadaMaquina "normal" _ = 3
jogadaMaquina "deus" 1 = 2
jogadaMaquina "deus" 2 = 3
jogadaMaquina "deus" 3 = 1
jogadaMaquina "deus" 4 = 5
jogadaMaquina "deus" 5 = 1
jogadaMaquina "tonto" 1 = 3
jogadaMaquina "tonto" 2 = 1
jogadaMaquina "tonto" 3 = 2
jogadaMaquina "tonto" 4 = 1
jogadaMaquina "tonto" 5 = 4
jogadaMaquina _ _ = 1

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
