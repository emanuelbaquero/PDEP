module Kars where


import Text.Show.Functions
import Data.List
import Data.Ord (comparing)

cantincrVelocidadNitro :: Double
cantincrVelocidadNitro = 15

type Truco = Auto -> Auto --Todos los trucos reciben un auto y devuelven un auto
data Auto = Auto {
    nombre :: [Char],
    nafta :: Double,
    velocidad :: Double, --Como la nafta es double, necesito que sea double para el truco turbo
    enamorado :: [Char],
    truco :: Truco
} deriving (Show)

rayo = Auto {
    nombre = "RochaMcQueen",
    nafta = 300.0,
    velocidad = 0,
    enamorado = "Ro",
    truco = deReversa
} 

biankerr = Auto {
    nombre = "Biankerr",
    nafta = 500.0,
    velocidad = 20,
    enamorado = "Tinch",
    truco = impresionar
}

gushtav = Auto {
    nombre = "Gushtav",
    nafta = 200.0,
    velocidad = 130,
    enamorado = "PetiLaLinda",
    truco = nitro
}

rodra = Auto {
    nombre = "Rodra",
    nafta = 0,
    velocidad = 50,
    enamorado = "Taisa",
    truco = fingirAmor "Petra"
}

--Trucos
deReversa :: Auto -> Auto
deReversa (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre (nafta + (velocidad * 0.2)) velocidad enamorado funcion)

impresionar :: Auto -> Auto
impresionar (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta (velocidad * 2) enamorado funcion)

nitro :: Auto -> Auto
nitro (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta (cantincrVelocidadNitro + velocidad) enamorado funcion)

fingirAmor :: [Char] -> Auto -> Auto
fingirAmor nuevoEnamorado auto= auto {enamorado = nuevoEnamorado, nafta = 0}

esVocal :: Char -> Bool
esVocal x = (elem) x "aeiou"

filtroVocales :: [Char] -> [Char] 
filtroVocales = filter esVocal

cantVocales :: [Char] -> Double
cantVocales = genericLength . filtroVocales

criterioVelocidad :: Double -> Double
criterioVelocidad x | x<=2 = 15
                    | x<=5 = 20
                    | otherwise = 30

cantIncrVelocidadAumento :: [Char] -> Double
cantIncrVelocidadAumento = criterioVelocidad . cantVocales

aumentarVelocidad :: Auto -> Auto
aumentarVelocidad (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta ((+) (cantIncrVelocidadAumento enamorado) velocidad ) enamorado funcion)


comboLoco :: Auto -> Auto
comboLoco = nitro . deReversa

queTrucazo :: Auto -> [Char] -> Auto
queTrucazo auto nuevoEnamorado = (aumentarVelocidad . (fingirAmor nuevoEnamorado)) auto

turbo :: Auto -> Auto
turbo (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre 0 ((nafta * 10 ) + velocidad) enamorado funcion)

--Utilizar el truco asignado al auto
utilizarTruco :: Auto -> Auto
utilizarTruco (Auto nombre nafta velocidad enamorado funcion) = funcion (Auto nombre nafta velocidad enamorado funcion)

--Valida si puede utilizar el truco
puedeUsarTruco :: Auto -> Bool
puedeUsarTruco (Auto {nafta = nafta, velocidad = velocidad}) = (nafta > 0) && (velocidad < 100)


--data carrera y trampas
type Trampa = Carrera -> Carrera --tbd
data Carrera = Carrera {
    cantVueltas :: Int,
    longPista :: Double,
    publico :: [[Char]],
    trampa :: Trampa,
    participantes:: [Auto]
} deriving (Show)

potrero = Carrera {
    cantVueltas = 3,
    longPista = 5.0,
    publico = ["Ronco", "Tinch", "Dodain"],
    trampa = sacarAlPistero,
    participantes = [rayo, biankerr, gushtav, rodra]
}

sacarAlPistero :: Trampa
sacarAlPistero carrera = carrera {participantes = ((tail . participantes) carrera)}

restarVelocidad :: Auto -> Auto
restarVelocidad auto = auto {velocidad = (velocidad auto) - 10}

lluvia :: Carrera -> Carrera
lluvia carrera = carrera {participantes = map restarVelocidad (participantes carrera)}

cambiarTruco :: Auto -> Auto
cambiarTruco auto = auto {truco = id} --le asigno la funcion identidad

neutralizarTrucos :: Carrera -> Carrera
neutralizarTrucos carrera = carrera {participantes = map cambiarTruco (participantes carrera)}

filtroNafta :: Auto -> Bool 
filtroNafta = (>30.0) . nafta --criterio de filtro

filtrarPorNafta :: [Auto] -> [Auto]
filtrarPorNafta = filter filtroNafta --podria llegar a ser redundante

pocaReserva :: Carrera -> Carrera
pocaReserva carrera = carrera {participantes = filtrarPorNafta (participantes carrera)}

podio :: Carrera -> Carrera
podio carrera = carrera {participantes = init (participantes carrera)} -- init devuelve la lista sin el ultimo elemento 

restarNafta :: Carrera -> Auto -> Auto
restarNafta carrera (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre (nafta - (((longPista carrera) / 10) * velocidad)) velocidad enamorado funcion) --revisar

enamoradoPresente :: Auto -> Carrera -> Bool
enamoradoPresente auto carrera = elem (enamorado auto) (publico carrera)

puedeUtilizarTrucoEnCarrera :: Auto -> Carrera -> Bool
puedeUtilizarTrucoEnCarrera auto carrera = (enamoradoPresente auto carrera) && (puedeUsarTruco auto)

utilizarTrucoEnCarrera :: Carrera -> Auto -> Auto
utilizarTrucoEnCarrera carrera auto  | puedeUtilizarTrucoEnCarrera auto carrera = utilizarTruco auto
                                     | otherwise = id auto


quienGana:: Carrera -> Auto
quienGana carrera = maximumBy (comparing velocidad) (participantes carrera)

elGranTruco :: Auto -> [(Auto -> Auto)] -> Auto
elGranTruco auto [] = auto
elGranTruco auto (cabeceraLista:colaLista) = elGranTruco (cabeceraLista auto) colaLista

listaFunciones :: [(Carrera -> Auto -> Auto)]
listaFunciones = [restarNafta, utilizarTrucoEnCarrera]

aplicarCambiosEnParticipantes :: [(Carrera -> Auto -> Auto)] -> Carrera -> Carrera
aplicarCambiosEnParticipantes [] carrera  = carrera
aplicarCambiosEnParticipantes (funcion:listaFunciones) carrera= aplicarCambiosEnParticipantes listaFunciones (carrera {participantes = map (funcion carrera) (participantes carrera)})

darVueltaCarrera :: Carrera -> Carrera
darVueltaCarrera = sacarAlPistero . (aplicarCambiosEnParticipantes listaFunciones)

darDosVueltas :: Carrera -> Carrera
darDosVueltas = darVueltaCarrera . darVueltaCarrera

correrCarrera :: Carrera -> Carrera
correrCarrera carrera = iterate (darVueltaCarrera) carrera !! (cantVueltas carrera)

--funciones para CPs
cantParticipantesPostTrampa =(length . participantes) . sacarAlPistero

rayoPresente coche = (nombre coche == nombre rayo)

rodraPresente coche = (nombre coche == nombre rodra)

ultimoParticipante = (velocidad . (last . participantes)) . lluvia

naftaPrimerParticipanteUnaVuelta = (nafta . (head . participantes)) . darVueltaCarrera

velocidadPrimerParticipanteUnaVuelta = (velocidad . (head . participantes)) . darVueltaCarrera

naftaPrimerParticipanteDosVueltas = (nafta . (head . participantes)) . darDosVueltas

velocidadGranTruco auto listaFunciones = (velocidad . (elGranTruco auto)) listaFunciones

naftaGranTruco auto listaFunciones = (nafta . (elGranTruco auto)) listaFunciones

participantesInfinitos :: [a] -> [a]
participantesInfinitos (x:xs) -> x:xs:carreraInfinita (x:xs)

 carreraInfinita :: Carrera -> Carrera
carreraInfinita (Carrera cantVueltas longPista publico trampa participantes) = (Carrera cantVueltas longPista publico trampa participantesInfinitos(participantes))   

 darVueltaCarreraInfinita :: Carrera -> Carrera 
darVueltaCarreraInfinita carrera = ((sacarAlPistero . (aplicarCambiosEnParticipantes listaFunciones)).(carreraInfinita)) carrera

 correrCarreraInfinita :: Carrera -> Carrera 
correrCarreraInfinita carrera = iterate (darVueltaCarrera.carreraInfinita) carrera !! (cantVueltas carrera)

