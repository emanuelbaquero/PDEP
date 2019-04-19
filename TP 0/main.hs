type TCoche = (String, Double, Int, String)
data DCoche = DCoche TCoche
--coches
rayo = DCoche ("RochaMcQueen", 300, 0 , "Ro")
biankerr = DCoche ("Biankerr", 500, 20, "Tinch")
gushtav = DCoche ("Gushtav", 200, 130, "PetiLaLinda")
rodra = DCoche ("Rodra", 0, 50, "Taisa")

--gets
getName (DCoche (name, _ , _,_)) = name
getNafta (DCoche (_, nafta, _,_)) = nafta
getVelocidad (DCoche (_, _, velocidad, _)) = velocidad
getEnamorado (DCoche (_, _, _, enamorado)) = enamorado
getEnamoradoCantChars = length . getEnamorado
getCantIncrVelocidad = cantIncrVelocidad . getEnamoradoCantChars



cantIncrVelocidad x | x<=2 = 15
                    | x<=5 = 20
                    | otherwise = 30
deReversa = ((+) (1000 * 0.2)) . getNafta
impresionar = ((*) 2) . getVelocidad
nitro = ((+) 15) . getVelocidad
incrementarVelocidad coche = (+) (getCantIncrVelocidad coche) (getVelocidad coche)

--TODO: fingirAmor
