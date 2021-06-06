module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Punto 1
data Guantelete = Guantelete {
    material :: String
    ,gemas :: [GemasDelInfinito]
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
guanteletePrueba = Guantelete "uru" [alma "Cantar", espacio "Jupiter", tiempo]

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
esUniversoAptoPendex :: Universo -> Bool
esUniversoAptoPendex = any cumpleEdad 

cumpleEdad :: Personaje -> Bool
cumpleEdad = (< 45) . edad 

energiaTotalUniverso :: Universo -> Number
energiaTotalUniverso = sum . energiasIntegrantes

energiasIntegrantes :: [Personaje] -> [Number]
energiasIntegrantes universo = map (energia) (filtrarHabitantesConMasDe1Habilidad universo)

filtrarHabitantesConMasDe1Habilidad :: Universo -> Universo
filtrarHabitantesConMasDe1Habilidad = filter (masDe1Habilidad 1)  --PARA HACER LAS COSAS MAS GENERICAS

masDe1Habilidad :: Number -> Personaje -> Bool
masDe1Habilidad minimo = (> minimo ) . length . habilidades

--Punto 3 
type Enemigo = Personaje
type GemasDelInfinito = Enemigo -> Enemigo

--MENTE
mente :: Number -> GemasDelInfinito
mente = debilitarEnergia 

debilitarEnergia :: Number -> Enemigo -> Enemigo
debilitarEnergia valor enemigo = enemigo { energia = (energia enemigo) - valor }

--ALMA
type Habilidad = String

alma :: Habilidad -> GemasDelInfinito
alma habilidad enemigo = ((debilitarEnergia 10) . (modificarHabilidadEnemigo enemigo) . (nuevasHabilidades habilidad)) enemigo

nuevasHabilidades :: Habilidad -> Enemigo -> [Habilidad]
nuevasHabilidades habilidad enemigo = filter (/= habilidad) (habilidades enemigo)

modificarHabilidadEnemigo ::  Enemigo -> [Habilidad] -> Enemigo
modificarHabilidadEnemigo enemigo habilidadesNuevas = enemigo {habilidades = habilidadesNuevas }

--ESPACIO
type Planeta = String

espacio :: Planeta -> GemasDelInfinito
espacio planeta = (debilitarEnergia 20) . (modificarPlaneta planeta)

modificarPlaneta :: Planeta -> Enemigo -> Enemigo
modificarPlaneta planeta enemigo = enemigo { planeta = planeta}

--PODER
poder :: GemasDelInfinito
poder enemigo  = ((debilitarEnergia (energia enemigo)) . (modificarHabilidadEnemigo enemigo) . quitarHabilidades . habilidades) enemigo 

quitarHabilidades :: [Habilidad] -> [Habilidad]
quitarHabilidades habilidades   | ((<= 2) . length) habilidades = []
                                | otherwise = habilidades

--TIEMPO
tiempo :: GemasDelInfinito
tiempo = (debilitarEnergia 50) . (reducirEdad) 

reducirEdad :: Enemigo -> Enemigo
reducirEdad enemigo = enemigo { edad = (max 18  . div (edad enemigo)) 2 }

--GEMA LOCA
loca :: GemasDelInfinito -> GemasDelInfinito
loca gema = gema . gema 


--Punto 4
{-Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de 
“usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell
-}

guanteleteEjemplo = Guantelete "goma" [tiempo, alma "usar Mjolnir", loca (alma "programacion en Haskell")]


--Punto 5
{-
No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y un enemigo 
ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el 
“efecto de lado” sobre la víctima.
-}

utilizar :: [GemasDelInfinito] -> Enemigo -> Enemigo
utilizar listaGemas enemigo = foldl ejecutarPoder enemigo listaGemas

ejecutarPoder :: Enemigo -> GemasDelInfinito -> Enemigo
ejecutarPoder enemigo gema = gema enemigo

--El "efecto de lado" sobre la victima se produce de forma que el foldl toma la primera gema de la lista y lo 
-- ejecuta sobre la semilla que en nuestro caso es el enemigo. Luego, sobre el nuevo enemigo (que tiene distintos atributos
-- que los que tenia la semilla por el efecto que tuvo sobre ella el primer poder) se vuelve
-- a ejecutar la proxima gema que esta en la lista. Asi vamos acumulando los efectos de las gemas sobre el enemigo. 
--RECORDAR NO DECIR NUNCA SE MODIFICA PORQ HASKELL NO MODIFICA SINO QUE CREA NUEVOS MUNDOS. 

--PUNTO 6
{-
Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona 
obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 
-}

gemaMasPoderosa :: Guantelete -> Personaje -> GemasDelInfinito
gemaMasPoderosa guantelete personaje = foldl1 (pierdeMasEnergia personaje energia) (gemas guantelete)

pierdeMasEnergia :: Personaje -> (Personaje -> Number) -> GemasDelInfinito -> GemasDelInfinito -> GemasDelInfinito
pierdeMasEnergia personaje energia gema1 gema2 | (energia . (ejecutarPoder personaje)) gema1 < energia (ejecutarPoder personaje gema2) = gema1
                                               | otherwise = gema2

--Como hay que hacerlo con recursividad
{-
gemaMasPoderosa :: Guantelete -> Personaje -> GemasDelInfinito
gemaMasPoderosa guantelete personaje = gemaDeMayorPoder personaje (gemas guantelete)

gemaDeMayorPoder :: Personaje -> [GemasDelInfinito] -> GemaDelInfinito
gemaDeMayorPoder _ [gema] = gema  (Lista de un elemento)
gemaDeMayorPoder personaje ( gema1 : gema2 : gemas) | energia (ejecutarPoder personaje gema1) > energia (ejecutarPoder personaje gema2) = gemaDeMayorPoder personaje (gema2 : gemas) 
                                                    | otherwise =  gemaDeMayorPoder personaje (gema1 : gemas) 
-}

