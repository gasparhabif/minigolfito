module Library where
import PdePreludat

-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-- Punto 1.a
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = Tiro {
    velocidad = 10,
    altura = 0,
    precision = (2*) $ precisionJugador unaHabilidad
} 

madera :: Palo
madera habilidad = Tiro {
    velocidad = 100,
    altura = 5,
    precision = precisionJugador habilidad / 2
}

hierro :: Number -> Palo
hierro n habilidad = Tiro {
    velocidad = n * fuerzaJugador habilidad,
    precision = precisionJugador habilidad / n,
    altura = max 0 (n-3)
}

-- Punto 1.b
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- Punto 2

golpe :: Palo -> Jugador -> Tiro
golpe palo = palo.habilidad

-- Punto 3

type Obstaculo = Tiro -> Tiro

superaObstaculo :: Bool -> Tiro -> Tiro
superaObstaculo criterio potenciador | criterio = potenciador
                                     | otherwise = tiroFrenado

tiroFrenado :: Tiro
tiroFrenado = Tiro {
    velocidad = 0,
    altura = 0,
    precision = 0
}

tunelConRampita :: Obstaculo
tunelConRampita tiro = superaObstaculo (criterioTunel tiro) (duplicarVelocidadTiro tiro)

criterioTunel :: Tiro -> Bool
criterioTunel = (> 90).precision

duplicarVelocidadTiro :: Tiro -> Tiro
duplicarVelocidadTiro tiro = tiro {
    velocidad = velocidad tiro * 2
}


laguna :: Number -> Obstaculo
laguna largoLaguna tiro = superaObstaculo (condicionLaguna tiro) (potenciadorLaguna largoLaguna tiro)

condicionLaguna :: Tiro -> Bool
condicionLaguna tiro = (&&) ((> 80).velocidad $ tiro) (between 1 5 (altura tiro))

potenciadorLaguna :: Number -> Tiro -> Tiro
potenciadorLaguna largoLaguna tiro = tiro {
    altura = altura tiro / largoLaguna
}

-- En ambos casos todas sus componentes quedan en 0 por lo que se podria directamente retornar tiroFrenado
hoyo :: Obstaculo
hoyo tiro = superaObstaculo (condicionHoyo tiro) (tiroFrenado)

condicionHoyo :: Tiro -> Bool
condicionHoyo tiro = (between 5 20 (velocidad tiro)) && ((>95).precision $ tiro) && ((0==).altura $ tiro)


-- Punto 4

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles persona obstaculo = filter (\palo -> (obstaculo $ golpe palo persona) /= tiroFrenado) palos

cuantosObstaculos :: [Obstaculo] -> Tiro -> Number
cuantosObstaculos obstaculos tiro = length $ takeWhile (\obstaculo -> (obstaculo tiro) /= tiroFrenado) obstaculos   

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil persona obstaculos = maximoSegun (cuantosObstaculos obstaculos.(flip golpe) persona) palos

-- Punto 5
jugadores :: [(Jugador, Puntos)] -> [Jugador]
jugadores = map fst

puntos :: [(Jugador, Puntos)] -> [Puntos]
puntos = map snd

padresPerdedores :: [(Jugador, Puntos)] -> [String]
padresPerdedores puntajes = map padre (jugadoresQuePerdieron puntajes)

jugadoresQuePerdieron :: [(Jugador, Puntos)] -> [Jugador]
jugadoresQuePerdieron puntajes = filter ((/=) (determinarGanador puntajes)) (jugadores puntajes)

determinarGanador :: [(Jugador, Puntos)] -> Jugador
determinarGanador [unPuntaje] = fst unPuntaje
determinarGanador (p1:p2:puntajes) | snd p1 > snd p2 = determinarGanador (p1:puntajes)
                                   | otherwise = determinarGanador (p2:puntajes)