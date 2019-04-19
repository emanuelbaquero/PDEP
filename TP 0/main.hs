type TCoche = (String, Double, Int, String)
--coches
rayo = ("RochaMcQueen", 300.0, 0 , "Ro")
biankerr = ("Biankerr", 500.0, 20, "Tinch")
gushtav = ("Gushtav", 200.0, 130, "PetiLaLinda")
rodra = ("Rodra", 0, 50.0, "Taisa")

--gets
getName (name, _ , _,_) = name
getNafta (_, nafta, _,_) = nafta
getVelocidad (_, _, velocidad, _) = velocidad
getEnamorado (_, _, _, enamorado) = enamorado
getEnamoradoCantChars coche = (length) (getEnamorado coche) 
getCantIncrVelocidad coche = (cantIncrVelocidad) (getEnamoradoCantChars coche)

cantIncrVelocidad x | x<=2 = 15
                    | x<=5 = 20
                    | otherwise = 30


deReversa = ((+) (1000 * 0.2)) . getNafta
impresionar = ((*) 2) . getVelocidad
nitro = ((+) 15) . getVelocidad
incrementarVelocidad coche = (+) (getCantIncrVelocidad coche) (getVelocidad coche)
fingirAmor (a, b , c, d) r = getEnamorado((a, b, c, r))
