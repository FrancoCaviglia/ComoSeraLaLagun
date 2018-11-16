{- Grupo 34

Moretti Gianni Quimey, 152/16 
Caviglia Román Franco, 333/18
Vallejos Facundo Daniel, 778/18

Comisión VI -}

-- Aquí las definiciones, funciones y ejemplos.

type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33, 53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20, (-10)]]

sopa3 :: Tablero
sopa3 = [[10, 5, 15], [-1, 7, 2], [2, 12, 3]]

camino1 :: Camino 
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino 
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,2),(2,2),(3,2)]

-- Da la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Da la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posición de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posición esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 

-- A partir de acá lo nuestro -- 

-- 1) Devuelve el número más grande de un tablero dado. --

maximo :: Tablero -> Integer
maximo t = maximoDeLista (concat t)

maximoDeLista :: [Integer] -> Integer
maximoDeLista [x] = x
maximoDeLista (x:y:ys) | x > y     = maximoDeLista (x:ys)
                       | otherwise = maximoDeLista (y:ys)

-- Fin del ejercicio 1) --

-- 2) Devuelve el número que más veces aparece en un tablero dado. Si hay empate devuelve cualquiera de ellos. --

masRepetido :: Tablero -> Integer
masRepetido t = masRepetidoTupla (juntarContadores (añadirContador (ordenarLista (concat t))))

-- Esta función toma una lista de números y los ordena de menor a mayor.
-- Son dos, y utiliza una función definida en el ejercicio 1).

ordenarLista :: [Integer] -> [Integer]
ordenarLista l | (length l == 1) = l
               | otherwise = ordenarLista (quitar (maximoDeLista l) l) ++ [maximoDeLista l]

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = (x : quitar n xs)

-- Suma a cada elemento de una lista un contador

añadirContador :: [Integer] -> [(Integer, Integer)]
añadirContador [] = []
añadirContador (x:xs) = (x, 1) : añadirContador xs

-- Dada la lista de tuplas, esta función pone el contador según corresponda

juntarContadores :: [(Integer, Integer)] -> [(Integer, Integer)]
juntarContadores []   = []
juntarContadores [x]  = [x]
juntarContadores ((x1, y1):(x2, y2):xs) | x1 == x2   = juntarContadores ((x1, y1+y2):xs)
                                        | otherwise  = (x1,y1) : juntarContadores ((x2,y2):xs)

-- Dada la lista de tuplas, esta función toma el valor de aquella con el mayor contador

masRepetidoTupla :: [(Integer, Integer)] -> Integer                                  
masRepetidoTupla [(x1,y1)] = x1
masRepetidoTupla ((x1,y1):(x2,y2):ys) | (y1>y2)   = masRepetidoTupla ((x1,y1):ys)
                                      | otherwise = masRepetidoTupla ((x2,y2):ys)

-- Fin ejercicio 2) -- 

-- 3) Devuelve los números de los casilleros de un camino. --

numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino t [] = []
numerosDeCamino t (p:ps) = (valor t p) : (numerosDeCamino t ps)

-- Fin ejercicio 3) -- 

-- 4) Devuleve True si y solo si en un camino no aparecen números repetidos. --

caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t c = not (hayRepetidos (numerosDeCamino t c))

-- Se fija si hay repetidos en una lista

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = (elem x xs) || hayRepetidos xs

-- Fin del ejercicio 4) -- 

-- 5) Determina si los números de los casilleros de un camino forman un camino de Fibonacci. --

-- Esta implementación utiliza la función del item 3)

caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci t c = esFibonacci (numerosDeCamino t c)

-- Toma una lista y devuelve si es o no una sucesión de Fibonacci

esFibonacci :: [Integer] -> Bool
esFibonacci xs | length xs <= 2 = True
esFibonacci (x1:x2:x3:xs) = (x1 + x2 == x3) && esFibonacci (x2:x3:xs)

-- Fin del ejercicio 5) --

-- 6) Devuelve la lista de números contenidos en el camino de Fibonacci más largo del tablero. Si hay empate devuelve cualquiera de ellos. --

mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci t = numerosDeCamino t (caminoMasLargo (eliminarCaminosRepetidos (todosLosFibonacci t)) )

caminoMasLargo :: Conjunto Camino -> Camino
caminoMasLargo [c] = c
caminoMasLargo (c:cs) | cs == [] = c
                      | length c >= length (caminoMasLargo cs) = c
                      | otherwise = caminoMasLargo cs

-- Estas dos funciones arman todos los caminos de Fibonacci iniciando en cada posición del tablero, y las juntan

todosLosFibonacci :: Tablero -> Conjunto Camino
todosLosFibonacci t = todosLosFibonacciDesde (1,1) t

todosLosFibonacciDesde :: Posicion -> Tablero -> Conjunto Camino
todosLosFibonacciDesde (x,y) t | (x <= cantidadFilas t) && (y < cantidadColumnas t) = fibonacciPartiendoDe (x,y) t ++ todosLosFibonacciDesde (x,y+1) t
                               | (x < cantidadFilas t) && (y == cantidadColumnas t) = fibonacciPartiendoDe (x,y) t ++ todosLosFibonacciDesde (x+1,1) t
                               | (x == cantidadFilas t) && (y == cantidadColumnas t) = fibonacciPartiendoDe (x,y) t

-- Esta es la función importante, la que dada una posición empieza a generar todos los caminos de Fibonacci posibles a partir de ella.

fibonacciPartiendoDe :: Posicion -> Tablero -> Conjunto Camino
fibonacciPartiendoDe (x,y) t | (posValida t (x+1,y)) && (posValida t (x,y+1)) = fabricaFibonacci (x,y) (x+1,y) t ++ fabricaFibonacci (x,y) (x,y+1) t
                             | (posValida t (x+1,y)) = fabricaFibonacci (x,y) (x+1,y) t
                             | (posValida t (x,y+1)) = fabricaFibonacci (x,y) (x,y+1) t
                             | otherwise = [[(x,y)]]

fabricaFibonacci :: Posicion -> Posicion -> Tablero -> Conjunto Camino
fabricaFibonacci (x1, y1) (x2,y2) t = fabricaFibonacciDerecha (x1,y1) (x2,y2) t ++ fabricaFibonacciAbajo (x1,y1) (x2,y2) t

fabricaFibonacciDerecha :: Posicion -> Posicion -> Tablero -> Conjunto Camino
fabricaFibonacciDerecha (x1,y1) (x2,y2) t | not (posValida t (x2, y2+1)) = [[(x1,y1), (x2, y2)]]
                                          | (posValida t (x2, y2+1)) && (caminoDeFibonacci t [(x1,y1),(x2,y2),(x2, y2+1)]) = agregarAlComienzo (x1,y1) (fabricaFibonacci (x2,y2) (x2,y2+1) t) 
                                          | otherwise = [[(x1,y1), (x2, y2)]]

fabricaFibonacciAbajo :: Posicion -> Posicion -> Tablero -> Conjunto Camino
fabricaFibonacciAbajo (x1,y1) (x2,y2) t | not (posValida t (x2+1, y2)) = [[(x1,y1), (x2, y2)]]
                                        | (posValida t (x2+1, y2)) && (caminoDeFibonacci t [(x1,y1),(x2,y2),(x2+1, y2)]) =  agregarAlComienzo (x1,y1) (fabricaFibonacci (x2,y2) (x2+1,y2) t)
                                        | otherwise = [[(x1,y1), (x2, y2)]]

-- Esta función agrega una posición al comienzo de cada camino de una lista de caminos

agregarAlComienzo :: Posicion -> Conjunto Camino -> Conjunto Camino
agregarAlComienzo p [] = [] 
agregarAlComienzo p (c:cs) = ( p : c ) : agregarAlComienzo p cs

eliminarCaminosRepetidos :: Conjunto Camino -> Conjunto Camino
eliminarCaminosRepetidos [] = []
eliminarCaminosRepetidos (c:cs) | elem c cs = eliminarCaminosRepetidos cs
                                | otherwise = c : eliminarCaminosRepetidos cs

-- Fin del ejercicio 6) -- 

-- 7) Devuelve el conjunto de las listas de números de todos los caminos de Fibonacci de longitud k. --

secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t k = numerosDeMuchosCaminos t (eliminarCaminosRepetidos (permutarCaminosKLista k (descartaLasMenores k (todosLosFibonacci t))))

-- Dados todos los caminos, elimina los que son de longitud menor a k.

descartaLasMenores :: Integer -> Conjunto Camino -> Conjunto Camino
descartaLasMenores k [] = []
descartaLasMenores k (c:cs) | fromIntegral (length c) >= k =  c : descartaLasMenores k cs
                            | otherwise = descartaLasMenores k cs

-- Esta función toma una lista de caminos de longitud mayor o igual a k, y devuelve de cada uno todas sus 'permutaciones' de k posiciones

permutarCaminosKLista :: Integer -> Conjunto Camino -> Conjunto Camino
permutarCaminosKLista k [] = []
permutarCaminosKLista k (c:cs) = (permutarCaminosK k c) ++ (permutarCaminosKLista k cs)

-- Esta función toma un camino de longitud mayor o igual a k, y devuelve todas sus 'permutaciones' de k posiciones

permutarCaminosK :: Integer -> Camino -> Conjunto Camino
permutarCaminosK k c | (length c) > (fromInteger k) = (generarListaLongitudK k c) : (permutarCaminosK k (tail c))
                     | (length c) == (fromInteger k) = [c]

-- Las siguientes dos funciones toman una lista de longitud mayor a k, y devuelven una lista de longitud k recortando lo que sobra al final
-- La idea es que para cada Camino, la función le saqué el primer elemento k veces, obteniendo una lista de longitud K

generarListaLongitudKDesde :: Integer -> Integer -> Camino -> Camino
generarListaLongitudKDesde n k c | n <= k = (head c) : generarListaLongitudKDesde (n+1) k (tail c)
                                 | otherwise = []

generarListaLongitudK :: Integer -> Camino -> Camino
generarListaLongitudK k c = generarListaLongitudKDesde 1 k c

-- Dados todos los caminos, los traduce en sus valores. Utiliza la función definida en el item 3).

numerosDeMuchosCaminos :: Tablero -> Conjunto Camino -> Conjunto [Integer]
numerosDeMuchosCaminos t [] = []
numerosDeMuchosCaminos t (c:cs) = numerosDeCamino t c : numerosDeMuchosCaminos t cs

-- Fin problema 7)