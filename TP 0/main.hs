type TCoche = ([Char], Double, Integer, [Char]) --(Nombre, Nafta, Velocidad, Enamorado)
name :: TCoche -> String
nafta :: TCoche -> Double
velocidad :: TCoche -> Integer
enamorado :: TCoche -> String
enamoradoCantChars  :: TCoche -> Int
cantIncrVelocidad :: TCoche -> Integer
criterioVelocidad :: (Num d, Ord a, Num a) => a -> d
deReversa :: TCoche -> Double
impresionar :: TCoche -> Integer
incrementarVelocidad :: TCoche -> Integer
fingirAmor :: TCoche -> String -> TCoche
mostrarAmorFingido :: TCoche -> String -> String

--coches
rayo = ("RochaMcQueen", 300.0, 0 , "Ro")
biankerr = ("Biankerr", 500.0, 20, "Tinch")
gushtav = ("Gushtav", 200.0, 130, "PetiLaLinda")
rodra = ("Rodra", 0.0, 50, "Taisa")

--gets
name (name, _ , _,_) = name
nafta (_, nafta, _,_) = nafta
velocidad (_, _, velocidad, _) = velocidad
enamorado (_, _, _, enamorado) = enamorado
enamoradoCantChars coche = (length) (enamorado coche) 
cantIncrVelocidad coche = (criterioVelocidad) (enamoradoCantChars coche)

criterioVelocidad x | x<=2 = 15
                    | x<=5 = 20
                    | otherwise = 30


deReversa = ((+) (1000 * 0.2)) . nafta
impresionar = ((*) 2) . velocidad
nitro = ((+) 15) . velocidad
incrementarVelocidad coche = (+) (cantIncrVelocidad coche) (velocidad coche)
fingirAmor (a, b , c, d) r =(a, b, c, r)
mostrarAmorFingido coche nuevoEnamorado = (enamorado . fingirAmor coche) nuevoEnamorado 