module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]

type Carta = (Number, String)

type Mano = [Carta]

data Jugador = Jugador {
    nombre :: String,
    cartasEnMano :: Mano,
    bebida :: String
}

--1a
mayorSegun f a b |f a > f b = a
                 |otherwise = b

--1b
maximoSegun _ [x] = x
maximoSegun f (x:y:xs) = maximoSegun f (mayorSegun f x y :xs)
