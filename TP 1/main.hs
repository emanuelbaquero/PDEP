import Text.Show.Functions
import Data.List
longPista:: Double
longPista = 1000

cantincrVelocidadNitro :: Double
cantincrVelocidadNitro = 15

type Truco = Auto -> Auto  --Todos los trucos reciben un auto y devuelven un auto
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

sumarNafta :: Double -> Double
sumarNafta = (+) (longPista * 0.2)

deReversa :: Auto -> Auto
deReversa (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre (sumarNafta nafta) velocidad enamorado funcion)

duplicarVelocidad:: Double -> Double
duplicarVelocidad = (*) 2

impresionar :: Auto -> Auto
impresionar (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta (duplicarVelocidad velocidad) enamorado funcion)

sumar:: Double -> Double -> Double
sumar cantidad = (+) cantidad

nitro :: Auto -> Auto
nitro (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta (sumar cantincrVelocidadNitro velocidad) enamorado funcion)

fingirAmor :: [Char] -> Auto -> Auto
fingirAmor nuevoEnamorado auto= auto {enamorado = nuevoEnamorado}

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
turbo (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre 0 ((+) ((* 10) nafta) velocidad) enamorado funcion)

--Utilizar el truco asignado al auto
utilizarTruco :: Auto -> Auto
utilizarTruco (Auto nombre nafta velocidad enamorado funcion) = funcion (Auto nombre nafta velocidad enamorado funcion)

--Valida si puede utilizar el truco
puedeUsarTruco :: Auto -> Bool
puedeUsarTruco (Auto {nafta = gas, velocidad = speed }) | (gas > 0) == (speed < 100) = True
                                                        | otherwise = False
