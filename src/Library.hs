module Library where
import PdePreludat

palos = ["Corazones", "Picas", 
         "Tréboles", "Diamantes"]

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

maximoSegun' funcion lista = foldl1 (mayorSegun funcion) lista

sinRepetidos [] = []
sinRepetidos (x:xs) = x: (sinRepetidos.filter (/= x)) xs

esCartaValida (numero, palo) = elem palo palos && elem numero [1..13]

esoNoSeVale = not.esCartaValida

manoNegra jugador = ((/=5).length.cartasEnMano) jugador || any esoNoSeVale (cartasEnMano jugador)

ocurrenciasDe x = length . filter (== x)
concatenar = foldl (++) []

iguales n mano = any ((==n).flip ocurrenciasDe numeros) numeros
        where numeros = map fst mano

par = iguales 2
pierna = iguales 3
fullHouse mano = par mano && pierna mano

otro:: Mano -> Bool
otro _ = True