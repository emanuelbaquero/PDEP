
Punto 3.1
-----
|Caso de Prueba   	|   Comando ejecutado	| Resultado Esperado   	|  Resultado Obtenido 	|
|---	              |---	                |---	                  |---	                  |
| Consultar la nafta de RochaMcQueen luego de realizar su truco. | utilizarTruco rayo	|500|500|
| Consultar la velocidad de Biankerr luego de realizar su truco. | utilizarTruco biankerr |40|40|
| Consultar la velocidad de Gushtav luego de realizar su truco. | utilizarTruco |45|45|  
| Consultar la enamorada de Rodra cuando elige a Petra. | utilizarTruco rodra | Petra | Petra

Punto 3.2
-----
|Caso de Prueba   	|   Comando ejecutado	| Resultado Esperado   	|  Resultado Obtenido 	|
|---	              |---	                |---	                  |---	                  |
|Consultar la velocidad de RochaMcQueen luego de incrementar su velocidad. | aumentarVelocidad rayo |15|15| 
| Consultar la velocidad de Biankerr luego de incrementar su velocidad. | aumentarVelocidad biankerr |35|35|
| Consultar la velocidad de Gushtav luego de incrementar su velocidad. | aumentarVelocidad gushtav |150|150|
| Consultar la velocidad de Rodra luego de incrementar su velocidad. | aumentarVelocidad rodra |70|70|

Punto 3.3
-----
|Caso de Prueba   	|   Comando ejecutado	| Resultado Esperado   	|  Resultado Obtenido 	|
|---	              |---	                |---	                  |---	                  |
| Consultar si RochaMcQueen puede usar su truco. | puedeUsarTruco rayo | True | True |
| Consultar si Gushtav puede usar su truco. | puedeUsarTruco gushtav | False | False |
| Consultar si Rodra puede usar su truco. | puedeUsarTruco rodra | False | False|

Punto 3.4
-----
|Caso de Prueba   	|   Comando ejecutado	| Resultado Esperado   	|  Resultado Obtenido 	|
|---	              |---	                |---	                  |---	                  |
| Consultar la nafta de Rocha luego de realizar comboLoco | comboLoco rayo | 500 | 500 |
| Consultar la velocidad de Rocha luego de realizar comboLoco | comboLoco rayo | 15 | 15|
| Consultar la velocidad de Rodra luego de utilizar queTrucazo cambiando su enamorada a Murcielaguita | queTrucazo rodra "Murcielaguita" | 80 | 80 |
| Consultar la velocidad de Gushtav luego de utilizar turbo | turbo gushtav | 2130 | 2130|
| Consultar la nafta de Gushtav luego de utilizar turbo | turbo gushtav | 0 | 0 |
| Consultar la velocidad de Rodra luego de utilizar turbo | turbo rodra | 50 | 50 |
| Consultar la nafta de Rodra luego de utilizar turbo | turbo rodra | 0 | 0 |
