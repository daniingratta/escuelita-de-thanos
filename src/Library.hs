module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Punto 1
data Guantelete = Guantelete {
    material :: String
    ,gemas :: [String]
} deriving (Show)

data Personaje = Personaje {
    edad :: Number
    ,energia :: Number
    ,habilidades :: [String]
    ,nombre :: String
    ,planeta :: String
} deriving (Show)

type Universo = [Personaje]

ironMan = Personaje 8 10 ["NADAR", "cantar"] "IRONMAN" "JUPITER"
drStrange = Personaje 4 15 ["Jugar"] "drStrange" "JUPITER"
groot = Personaje 9 10 ["NADAR"] "groot" "JUPITER"
wolverine = Personaje 2 10 ["NADAR", "bailar"] "wolverine" "JUPITER"
viudaNegra = Personaje 1 10 ["NADAR"] "viudaNegra" "JUPITER"

universoPrueba = [ironMan, drStrange, groot, wolverine, viudaNegra]
guanteletePrueba = Guantelete "uru" ["f","f","a","gola","djd","dd"]

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universo |puedeChasquear guantelete = reducirUniverso universo
                                      | otherwise = universo
puedeChasquear :: Guantelete -> Bool
puedeChasquear guantelete = estaCompleto guantelete && cumpleMaterial guantelete

estaCompleto :: Guantelete -> Bool
estaCompleto = (==6) . length . gemas 

cumpleMaterial :: Guantelete -> Bool
cumpleMaterial = (=="uru") . material

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (div (habitantes universo) 2) universo

habitantes = length

--Punto 2
{-Resolver utilizando únicamente orden superior.
Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.
-}

type FuncionAny = Personaje -> Bool

esUniversoAptoPendex :: Universo -> FuncionAny -> Bool
esUniversoAptoPendex universo funcion = any funcion universo

cumpleEdad :: Personaje -> Bool
cumpleEdad = (< 45) . edad

type Energias =  [Personaje] -> [Number]

energiaTotalUniverso :: Energias -> Universo -> Number
energiaTotalUniverso energias = sum . energias

energiasIntegrantes :: [Personaje] -> [Number]
energiasIntegrantes universo = map (energia) (filtrarHabitantesConMasDe1Habilidad universo)

filtrarHabitantesConMasDe1Habilidad :: Universo -> Universo
filtrarHabitantesConMasDe1Habilidad = filter (masDe1Habilidad 1)  --PARA HACER LAS COSAS MAS GENERICAS

masDe1Habilidad :: Number -> Personaje -> Bool
masDe1Habilidad minimo = (> minimo ) . length . habilidades

--Punto 3 




