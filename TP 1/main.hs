import Text.Show.Functions
import Data.List
longPista = 1000
cantincrVelocidadNitro = 15

type Truco = Auto -> Auto  --Todos los trucos reciben un auto y devuelven un auto
data Auto = Auto {
    nombre :: [Char],
    nafta :: Double,
    velocidad :: Double,
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
    truco = fingirAmor
}

sumarNafta :: Double -> Double
sumarNafta = (+) (longPista * 0.2)
deReversa :: Auto -> Auto
deReversa (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre (sumarNafta nafta) velocidad enamorado funcion)


duplicarVelocidad:: Double -> Double
duplicarVelocidad = (*) 2
impresionar :: Auto -> Auto
impresionar (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta (duplicarVelocidad velocidad) enamorado funcion)

--sumarVelocidad:: Double -> Double -> Double
sumar cantidad = (+) cantidad
nitro :: Auto -> Auto
nitro (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre nafta (sumar cantincrVelocidadNitro velocidad) enamorado funcion)

fingirAmor :: Auto -> Auto
fingirAmor auto = auto {enamorado = "Petra"}

--esVocal :: [Char] -> [Char] -> Bool
esVocal x = (elem) x "aeiou"
--filtroVocales :: [Char] -> [Char] 
filtroVocales = filter esVocal

--cantVocales :: [Char] -> Int
cantVocales = genericLength . filtroVocales

criterioVelocidad :: Int -> Int
criterioVelocidad x | x<=2 = 15
                    | x<=5 = 20
                    | otherwise = 30

cantIncrVelocidadAumento = criterioVelocidad . cantVocales

nombreEnamorado (Auto {enamorado = x}) = x 

cantAumento :: Auto -> Int  --Devuelve la cantidad que debe aumentar en base a las vocales de su enamorado
cantAumento = cantIncrVelocidadAumento . nombreEnamorado


comboLoco = nitro . deReversa

--queTrucazo = fingirAmor . aumentarVelocidad 

turbo (Auto nombre nafta velocidad enamorado funcion) = (Auto nombre 0 ((+) ((* 10) nafta) velocidad) enamorado funcion)

--Utilizar el truco asignado al auto
utilizarTruco (Auto nombre nafta velocidad enamorado funcion) = funcion (Auto nombre nafta velocidad enamorado funcion)

--Valida si puede utilizar el truco
puedeUsarTruco (Auto {nafta = gas, velocidad = speed }) | (gas > 0) == (speed < 100) = True
                                                        | otherwise = False

--TODO: aumentarVelocidad, tipos por funcion