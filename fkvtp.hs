type Conjunto a = [a]
type Tablero = [[Integer]]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13, 12, 19, 6], [7, 13, 32, 6], [22, 20, 14, 7], [7, 33,53, 16], [27, 2, 8, 18]]

sopa2 :: Tablero
sopa2 = [[(-20), (-20), (-20)], [0, 10, 20], [(-10), (-10), 0], [10, 20,(-10)]]

sopa3 :: Tablero
sopa3 = [[10,5,15],[-1,7,2],[2,12,3]]

sopatest :: Tablero
sopatest = [[1,2],[3,4]]

camino1 :: Camino 
camino1 = [(1,1),(1,2),(2,2),(2,3)]

camino2 :: Camino 
camino2 = [(2,1),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,2),(2,2),(3,2)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 

---------1-------

maximo :: Tablero -> Integer
maximo t = maximoDeLista (concat t)

maximoDeLista :: [Integer] -> Integer
maximoDeLista [x] = x
maximoDeLista (x:y:ys) | x > y     = maximoDeLista (x:ys)
                       | otherwise = maximoDeLista (y:ys)

---------2--------

masRepetido :: Tablero -> Integer
masRepetido t = repetido (juntarTuplas (aTupla (ordenar (concat t))))

quitar n [] = []
quitar n (x:xs) | n==x = xs
                | otherwise = (x:quitar n xs)

ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar xs  = ordenar (quitar (maximoDeLista xs) xs) ++ [maximoDeLista xs]

aTupla :: [Integer] -> [(Integer, Integer)]
aTupla [] = []
aTupla (x:xs)= (x,1):(aTupla xs)

juntarTuplas :: [(Integer, Integer)] -> [(Integer, Integer)]
juntarTuplas []  = []
juntarTuplas [x] = [x]
juntarTuplas ((x1,x2):(y1,y2):ys) | x1==y1    = juntarTuplas ((x1,x2+y2):ys)    
                                  | otherwise = (x1,x2):juntarTuplas ((y1,y2):ys)

repetido :: [(Integer, Integer)] -> Integer                                  
repetido [(x1,x2)] = x1
repetido ((x1,x2):(y1,y2):ys) | (x2>y2)   = repetido ((x1,x2):ys)
                              | otherwise = repetido ((y1,y2):ys)
     
------------3----------

numerosDeCamino :: Tablero -> Camino -> [Integer]
numerosDeCamino t [] = []
numerosDeCamino t (c:cs) = (valor t c) : (numerosDeCamino t cs)

-----------4-----------

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | x `elem` xs = True
                    | otherwise   = hayRepetidos xs
                
caminoSinRepetidos :: Tablero -> Camino -> Bool
caminoSinRepetidos t c = not (hayRepetidos (numerosDeCamino t c))

-----------5-----------

caminoDeFibonacci :: Tablero -> Camino -> Bool
caminoDeFibonacci t c = esFibonacci (numerosDeCamino t c)

esFibonacci  xs        | length xs <= 2 = True
esFibonacci (x:y:z:zs) | (x+y==z)  = esFibonacci (y:z:zs)
                       | otherwise = False

------------6-----------

mayorSecuenciaDeFibonacci :: Tablero -> [Integer]
mayorSecuenciaDeFibonacci t = numerosDeCamino t (masLargo (eliminarRepetidosCamino (todosLosFibonacci t)) )

masLargo :: Conjunto Camino -> Camino
masLargo [c] = c
masLargo (c:cs) | cs == [] = c
                | length c >= length (masLargo cs) = c
                | otherwise = masLargo cs

-- Estas dos funciones arman todos los fibonacci iniciando en cada posición del tablero, y las juntan

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

eliminarRepetidosCamino :: Conjunto Camino -> Conjunto Camino
eliminarRepetidosCamino [c] = [c]
eliminarRepetidosCamino (c:cs) | pertenece_camino c cs = eliminarRepetidosCamino cs
                               | otherwise = c : eliminarRepetidosCamino cs

pertenece_camino :: Camino -> Conjunto Camino -> Bool
pertenece_camino n []     = False
pertenece_camino n (c:cs) = (n == c) || pertenece_camino n cs

----------------7------------------

secuenciasDeFibonacciDeLongitudK :: Tablero -> Integer -> Conjunto [Integer]
secuenciasDeFibonacciDeLongitudK t k = eliminarRepeticiones (secuenciasdeFibonacci t k)

secuenciasdeFibonacci :: Tablero -> Integer -> [[Integer]]
secuenciasdeFibonacci t n = caminosANumeros t (filtroPorLongitud (todosLosFibonacci t) n)

filtroPorLongitud :: [[(Integer, Integer)]] -> Integer -> [[(Integer, Integer)]]
filtroPorLongitud [] n = []
filtroPorLongitud xs 0 = []
filtroPorLongitud (x:xs) n | length x == (fromIntegral n) = x:(filtroPorLongitud xs n)
                           | otherwise = filtroPorLongitud xs n

eliminarRepeticiones :: [[Integer]] -> [[Integer]]
eliminarRepeticiones [] = []
eliminarRepeticiones (x:xs) | elem x xs = eliminarRepeticiones xs
                            | otherwise = x:(eliminarRepeticiones xs) 
                  
caminosANumeros :: Tablero -> [Camino] -> [[Integer]]
caminosANumeros t [] = []
caminosANumeros t (c:cs) = numerosDeCamino t c :(caminosANumeros t cs)