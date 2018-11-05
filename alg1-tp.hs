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

-- 1) Máximo de un tablero --

maximo :: Tablero -> Integer
maximo [x]    = maximoFila x
maximo (x:xs) | maximo_xs >= maximoFila_x  = maximo_xs
              | otherwise                  = maximoFila_x
              where maximo_xs = maximo xs
                    maximoFila_x = maximoFila x

maximoFila :: [Integer] -> Integer
maximoFila [x]    = x
maximoFila (x:xs) | maximoFila_xs > x  = maximoFila_xs 
                  | otherwise          = x
                  where maximoFila_xs = maximoFila xs

-- Fin del ejercicio 1) --

-- 2) Más repetido de un tablero --

masRepetido :: Tablero -> Integer
masRepetido t = auxMasRepetido (repeticiones (ordenar (numerosDeTablero t)))

masRepetidoTupla :: [(Integer, Integer)] -> Integer
masRepetidoTupla [] = 0
masRepetidoTupla (x:xs) | snd (head xs) <= snd x = fst x
                        | otherwise = masRepetidoTupla xs

-- Esta función toma un tablero y devuelve una lista con todos sus números

numerosDeTablero :: Tablero -> [Integer]
numerosDeTablero [x] = x
numerosDeTablero (x:xs) = x ++ numerosDeTablero xs

-- Esta función toma una lista de números y los ordena de menor a mayor.
-- Son tres

ordenar :: [Integer] -> [Integer]
ordenar l | [x] = [x]
          | otherwise = ordenar (quitar (maximo l) l) ++ [maximo l]

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) | x > head xs = maximo (x : (tail xs))
              | otherwise = maximo xs

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = (x : quitar n xs)

repeticiones :: [Integer] -> [(Integer, Integer)]
repeticiones x = auxRepeticiones (añadirContador x)

añadirContador :: [Integer] -> [(Integer, Integer)]
añadirContador []     = []
añadirContador (x:xs) = (x, 1) : añadirContador xs

-- Dada la lista de tuplas, esta función pone el contador según corresponda

auxRepeticiones :: [(Integer, Integer)] -> [(Integer, Integer)]
auxRepeticiones []   = []
auxRepeticiones [x]  = [x]
auxRepeticiones ((x1, y1):(x2, y2):xs) | x1 == x2   = auxRepeticiones ((x1, y1+y2):xs)
                                       | otherwise  = (x1,y1) : auxRepeticiones ((x2,y2):xs)

-- Fin ejercicio 2) -- 

-- 3) Números asociados a un camino --

numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino t [] = []
numerosDeCamino t (x:xs) = (valor t x) : (numerosDeCamino t xs)

-- Fin ejercicio 3) -- 

-- 4) Si no aparecen numeros repetidos en un camino devuelve True 

-- Esta implementación utiliza la función del item 3)

caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t c = not (hayRepetidos (numerosDeCamino t c))

-- Se fija si hay repetidos en una lista

hayRepetidos :: Camino -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = (pertenece x xs) || hayRepetidos xs

-- Si fija si algo pertenece a una lista

pertenece :: Integer -> [Integer] -> Bool
pertenece n []     = False
pertenece n (x:xs) = (n == x) || pertenece n xs

-- Fin del ejercicio 4) -- 

-- 5) Si los números de un camino siguen la sucesión de Fibonacci da True--

-- Esta implementación utiliza la función del item 3)

caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci t c = esFibonacci (numerosDeCamino t c)

-- Toma una lista y devuelve si es o no una sucesión de Fibonacci

esFibonacci :: [Integer] -> Bool
esFibonacci [] = True
esFibonacci [x1] = True
esFibonacci [x1, x2] = True
esFibonacci (x1:x2:x3:xs) = (x1 + x2 == x3) && esFibonacci (x2:x3:xs)

-- Fin del ejercicio 5) --

-- 6) Devuelve la lista de números contenidos en el camino de Fibonacci más largo del tablero.

{- 

variaciones :: [Posicion] -> Integer -> [[Posicion]]
variaciones ps 0  = [[]]
variaciones ps l  = auxVariaciones2 ps (variaciones ps (l-1))

auxVariaciones2 :: [Posicion] -> [[Posicion]] -> [[Posicion]]
auxVariaciones2 [] ys      = []
auxVariaciones2 (x:xs) ys  = (auxVariaciones1 x ys) ++ (auxVariaciones2 xs ys)

auxVariaciones1 :: Posicion -> [[Posicion]] -> [[Posicion]]
auxVariaciones1 p []      = []
auxVariaciones1 p (x:xs)  = [p : x] ++ (auxVariaciones1 p xs)

posiciones :: Tablero -> [Posicion]
posiciones t = productoCartesiano (ordenar (armarLista (cantidadFilas t))) (ordenar (armarLista (cantidadColumnas t)))

armarLista :: Integer -> [Integer]
armarLista 0 = []
armarLista n = n : armarLista (n-1)

crearTuplas :: Integer -> [Integer] -> [Posicion]
crearTuplas n []      = []
crearTuplas n (x:xs)  = (n, x) : (crearTuplas n xs)

productoCartesiano :: [Integer] -> [Integer] -> [Posicion]
productoCartesiano [] ys      = []
productoCartesiano (x:xs) ys  = (crearTuplas x ys) ++ (productoCartesiano xs ys)

-} 

mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci t = masLargo (todosLosFibonacci t)

masLargo :: Conjunto Camino -> Camino
masLargo (c:cs) | cs == [] = []
                | length c > length (másLargo cs) = c
                | otherwise = másLargo c

-- Estas dos funciones arman todos los fibonacci iniciando en cada posición del tablero, y las juntan

todosLosFibonacci :: Tablero -> Conjunto Camino
todosLosFibonacci t = todosLosFibonacciDesde (1,1) t

todosLosFibonacciDesde :: Posicion -> Tablero -> Conjunto Camino
todosLosFibonacciDesde (x,y) t | (x < cantidadFilas t) && (y <= cantidadColumnas t) = fibonacciPartiendoDe (x,y) ++ todasLasInicialesDesde (x+1,y)
                               | (x = cantidadFilas t) && (y < cantidadColumnas t)  = fibonacciPartiendoDe (x,y) ++ todasLasInicialesDesde (1,y+1)
                               | (x = cantidadFilas t) && (y = cantidadColumnas t)  = fibonacciPartiendoDe (x,y)

-- Esta es la función importante, la que dada una posición empieza a generar todos los caminos de Fibonacci posibles a partir de ella.

fibonacciPartiendoDe :: Posicion -> Tablero -> Conjunto Camino
fibonacciPartiendoDe (x,y) t | posValida (x+1,y) t && posValida (x,y+1) t = fabricaFibonacci (x+1, y) (x,y) t ++ fabricaFibonacci (x, y) (x,y+1) t
                             | posValida (x+1,y) = fabricaFibonacci (x, y) (x,y+1) t
                             | posValida (x,y+1) = fabricaFibonacci (x, y) (x,y+1) t
                             | otherwise = [[(x,y)]]

fabricaFibonacci :: Posicion -> Posicion -> Tablero -> Conjunto Camino
fabricaFibonacci (x1, y1) (x2,y2) t = fabricaFibonacciDerecha (x1, y1) (x2,y2) t ++ fabricaFibonacciAbajo (x1, y1) (x2,y2) t

fabricaFibonacciDerecha :: Posicion -> Posicion -> Tablero -> Conjunto Camino
fabricaFibonacciDerecha (x1,y1) (x2,y2) t | not posValida (x2+1, y2) = [[(x1,y1), (x2, y2)]]
                                          | posValida (x2+1, y2) && esFibonacci [(x1,y1) ++ (x2,y2) ++ (x2+1, y2)] = agregarAlComienzo [(x1,y1)] (fabricaFibonacci (x2,y2) (x2+1,y2) t) 

fabricaFibonacciAbajo :: Posicion -> Posicion -> Tablero -> Conjunto Camino
fabricaFibonacciAbajo (x1,y1) (x2,y2) t | not posValida (x2+1, y2) = [[(x1,y1), (x2, y2)]]
                                        | posValida (x2, y2+1) && esFibonacci [(x1,y1) ++ (x2,y2) ++ (x2, y2+1)] =  agregarAlComienzo [(x1,y1)] (fabricaFibonacciLargo (x2,y2) (x2,y2+1) t)
                       
-- Esta función agrega una posición al comienzo de cada camino de una lista de caminos

agregarAlComienzo :: Posicion -> Conjunto Camino -> Conjunto Camino
agregarAlComienzo (x,y) [c] = [(x,y) : c]
agregarAlComienzo (x,y) (c:cs) = [ (x,y) : c ] ++ agregarAlComienzo (x,y) cs

-- Fin del ejercicio 6) -- 

-- 7) Devuelve las secuencias de Fibonacci de longitud K

secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t n = eliminarRepetidos (permutarCaminosKLista k (descartaLasMenores k (recorreIniciales t)))

-- Dados todos los caminos, elimina los que son de longitud menor a K.

descartaLasMenores :: Integer -> Conjunto Camino -> Conjunto Camino
descartaLasMenores k c | c == [] = []
                       | length (head c) => k = (head c) ++ descartaLasMenores k (tail c)
                       | otherwise = descartaLasMenores k (tail c)

-- Las siguientes dos funciones toman una lista de longitud mayor a K, y devuelven una lista de longitud K recortando lo que sobra al final
-- Acá se hace el paso de Camino a [Interger] con los números del camino
-- La idea es que para cada Camino, la función le saqué el primer elemento K veces, obteniendo una lista de longitud K

generarListaLongitudKDesde :: Integer -> Integer -> Camino -> [Integer]
generarListaLongitudKDesde n k c | n <= k = (numerosDeCamino (head c)) : generarListaLongitudKDesde (n+1) k (tail c)
                                 | otherwise = []

generarListaLongitudK :: Integer -> Camino -> [Integer]
generarListaLongitudK k c = generarListaLongitudKDesde 1 k c

-- Esta función toma una lista de valores de un camino de longitud mayor o igual a K, y devuelve todas sus 'permutaciones' de K posiciones

permutarCaminosK :: Integer -> [Integer] -> Conjunto [Integer]
permutarCaminosK k c | leght c < k = (generarListaLongitudK k c) : (permutarCaminosK k (tail c))
                     | leght c == k = [c]

-- Hace lo mismo que la anterior pero con muchos caminos

permutarCaminosKLista :: Integer -> Conjunto Camino -> Conjunto [Integer]
permutarCaminosKLista k [c] = permutarCaminosK k c
permutarCaminosKLista k (c:cs) = (permutarCaminosK k c) ++ (permutarCaminosKLista cs)

-- Dada la lista final de valores de caminos todos de longitud K, elimina los repetidos
-- Esta implementación utiliza la función pertenece del ejercicio 4)

eliminarRepetidos :: Conjunto [Integer] -> Conjunto [Integer]
eliminarRepetidos [c] = [c]
eliminarRepetidos (c:cs) | pertenece c cs = eliminarRepetidos cs
                         | otherwise = c : eliminarRepetidos cs

-- Fin problema 7)
