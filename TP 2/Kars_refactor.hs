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
deReversa auto@(Auto _ nafta velocidad _ _) = auto {nafta = (nafta + (velocidad * 0.2))}

impresionar :: Auto -> Auto
impresionar auto@(Auto _ _ velocidad _ _) = auto {velocidad = velocidad * 2}

nitro :: Auto -> Auto
nitro auto@(Auto _ _ velocidad _ _) = auto {velocidad = (cantincrVelocidadNitro + velocidad)}

fingirAmor :: [Char] -> Auto -> Auto
fingirAmor nuevoEnamorado auto= auto {enamorado = nuevoEnamorado}


esVocal :: Char -> Bool
esVocal palabra = (elem) palabra "aeiou"

filtroVocales :: [Char] -> [Char] 
filtroVocales = filter esVocal

cantVocales :: [Char] -> Double
cantVocales = genericLength . filtroVocales

criterioVelocidad :: Double -> Double
criterioVelocidad cantVocales   | cantVocales<=2 = 15
                                | cantVocales<=5 = 20
                                | otherwise = 30

cantIncrVelocidadAumento :: [Char] -> Double
cantIncrVelocidadAumento = criterioVelocidad . cantVocales

aumentarVelocidad :: Auto -> Auto
aumentarVelocidad auto@(Auto _ _ velocidad enamorado _) = auto {velocidad = (cantIncrVelocidadAumento enamorado) + velocidad}


comboLoco :: Auto -> Auto
comboLoco = nitro . deReversa

queTrucazo :: Auto -> [Char] -> Auto
queTrucazo auto nuevoEnamorado = (aumentarVelocidad . (fingirAmor nuevoEnamorado)) auto

turbo :: Auto -> Auto
turbo auto@(Auto _ nafta velocidad _ _) = auto {nafta = 0, velocidad = ((nafta * 10 ) + velocidad)}

--Utilizar el truco asignado al auto
utilizarTruco :: Auto -> Auto
utilizarTruco auto@(Auto _ _ _ _ funcion) = funcion auto

--Valida si puede utilizar el truco
puedeUsarTruco :: Auto -> Bool
puedeUsarTruco (Auto {nafta = nafta, velocidad = velocidad}) = (nafta > 0) && (velocidad < 100)


--data carrera y trampas
type Trampa = Carrera -> Carrera 
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
sacarAlPistero carrera@(Carrera _ _ _ _ participantes) = carrera {participantes = tail participantes}

restarVelocidad :: Auto -> Auto
restarVelocidad auto@(Auto _ _ velocidad _ _) = auto {velocidad = velocidad - 10}

lluvia :: Carrera -> Carrera
lluvia carrera@(Carrera _ _ _ _ participantes) = carrera {participantes = map restarVelocidad participantes}

cambiarTruco :: Auto -> Auto
cambiarTruco auto = auto {truco = id} --le asigno la funcion identidad para simular que no realiza ninguna accion

neutralizarTrucos :: Carrera -> Carrera
neutralizarTrucos carrera@(Carrera _ _ _ _ participantes) = carrera {participantes = map cambiarTruco participantes }

filtroNafta :: Auto -> Bool 
filtroNafta = (>30.0) . nafta --criterio de filtro

filtrarPorNafta :: [Auto] -> [Auto]
filtrarPorNafta = filter filtroNafta --podria llegar a ser redundante

pocaReserva :: Carrera -> Carrera
pocaReserva carrera@(Carrera _ _ _ _ participantes) = carrera {participantes = filtrarPorNafta participantes}

podio :: Carrera -> Carrera
podio carrera@(Carrera _ _ _ _ participantes) = carrera {participantes = init participantes} -- init devuelve la lista sin el ultimo elemento 

restarNafta :: Carrera -> Auto -> Auto
restarNafta (Carrera _ longPista _ _ _) auto@(Auto _ nafta velocidad _ _) = auto {nafta = (nafta - ((longPista / 10) * velocidad))} --averiguar si es valido

enamoradoPresente :: Auto -> Carrera -> Bool
enamoradoPresente (Auto _ _ _ enamorado _) (Carrera _ _ publico _ _) = elem enamorado publico

puedeUtilizarTrucoEnCarrera :: Auto -> Carrera -> Bool
puedeUtilizarTrucoEnCarrera auto carrera = (enamoradoPresente auto carrera) && (puedeUsarTruco auto)

utilizarTrucoEnCarrera :: Carrera -> Auto -> Auto
utilizarTrucoEnCarrera carrera auto  | puedeUtilizarTrucoEnCarrera auto carrera = utilizarTruco auto
                                     | otherwise = id auto


quienGana:: Carrera -> Auto
quienGana carrera@(Carrera _ _ _ _ participantes) = maximumBy (comparing velocidad) participantes

elGranTruco :: Auto -> [(Auto -> Auto)] -> Auto
elGranTruco auto [] = auto
elGranTruco auto (cabeceraLista:colaLista) = elGranTruco (cabeceraLista auto) colaLista

listaFuncionesVuelta :: [(Carrera -> Auto -> Auto)]
listaFuncionesVuelta = [restarNafta, utilizarTrucoEnCarrera]

aplicarCambiosEnParticipantes :: [(Carrera -> Auto -> Auto)] -> Carrera -> Carrera
aplicarCambiosEnParticipantes [] carrera  = carrera
aplicarCambiosEnParticipantes (funcion:listaFuncionesVuelta) carrera@(Carrera _ _ _ _ participantes)= aplicarCambiosEnParticipantes listaFuncionesVuelta (carrera {participantes = map (funcion carrera) participantes})

darVueltaCarrera :: Carrera -> Carrera
darVueltaCarrera = sacarAlPistero . (aplicarCambiosEnParticipantes listaFuncionesVuelta)

darDosVueltas :: Carrera -> Carrera
darDosVueltas = darVueltaCarrera . darVueltaCarrera

correrCarrera :: Carrera -> Carrera
correrCarrera carrera@(Carrera cantVueltas _ _ _ _) = iterate darVueltaCarrera carrera !! cantVueltas



