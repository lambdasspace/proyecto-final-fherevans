
module EspacioVectorial where

-- Creamos una clase de tipos para todos los elementos que puedan ser parte de un espacio vectorial
-- Todo espacio vectorial debe implementar la operación de suma de vectores y multiplicación por escalares.
-- Un EV tiene un elemento cero, hará falta especificar la dimensión del EV para obtener dicho vector 
class EV a where
    suma :: (Fractional e) => (a e) -> (a e) -> (a e)
    multiplica :: (Fractional e) => e -> (a e) -> (a e)

-- tipo para las n-adas
data Vector a = Vector [a]

-- Instanciamos show
instance (Show a) => Show (Vector a) where
    show (Vector ls) = "(" ++ (showaux ls) ++ ")"
    
showaux :: (Show a) => [a] -> String
showaux [x] = show x
showaux (x:xs) = (show x) ++ ", " ++ showaux xs

-- definimos Vector como instancia de la clase EV
instance EV Vector where
    suma (Vector x) (Vector y) = if verifica x y then Vector (zipWith (+) x y) else error "Solo se pueden sumar vectores del mismo espacio vectorial"
    multiplica k x = fmap (*k) x

-- Instanciamos functor para usar fmap
instance Functor Vector where
    fmap f (Vector x) = Vector (map f x)
    
instance (Eq a) => Eq (Vector a) where
    (Vector x) == (Vector y) = x == y

-- Función auxiliar que verifica si dos vectores pertenecen al mismo EV
verifica :: [a] -> [a] -> Bool
verifica a b = (length a) == (length b) 

-- Definir producto punto para vectores
productoPunto :: (Num a) => Vector a -> Vector a -> a
productoPunto (Vector v) (Vector u) = if verifica v u then foldl (+) 0 $ zipWith (*) v u else error "Vecotres de distintos espacios vectoriales"

-- Retorna la lista que corresponde a los valores del vector
getVector:: Vector a -> [a]
getVector (Vector x) = x

-- Función que nos otorga el cero de un EV de dimensión N
ceroVector :: Int -> Vector Double
ceroVector n = Vector (take n [0.0,0.0..])

-- *** -- *** -- *** -- *** -- *** -- *** -- *** -- *** -- ***

-- Tipo de dato matríz, puede definir una materíz por fila o una matríz por columna
data Matriz a = Matriz [Vector a]

-- Instanciamos Functor para matrices
instance Functor Matriz where
    fmap f (Matriz a) = Matriz (map (\x -> fmap f x) a)

--Instanciamos Show
instance (Show a) => Show (Matriz a) where 
    show (Matriz []) = ""
    show (Matriz (x:xs)) = "|" ++ (show x) ++ "|\n" ++ show (Matriz xs)

-- Instanciamos a matriz como un espacio vectorial
instance EV Matriz where
    suma (Matriz xs) (Matriz ys) = if verificaSum xs ys then let p = zip xs ys 
                                                                in Matriz (map (\(a,b) -> suma a b) p)
                                    else error "No es posible sumar matrices de distintos EV"
    multiplica a m = fmap (*a) m

-- Función auxiliar con la cuál verificamos si un par de matrices tienen la misma dimensión
verificaSum :: [Vector a] -> [Vector a] -> Bool 
verificaSum xs ys = if (length xs) == (length ys) then let p = zip xs ys in
                                                            and $ map (\(a,b) -> (length $ getVector a)==(length $ getVector b)) p
                                                else False
--Devuelve los vecotres
getFilas :: Matriz a -> [Vector a]
getFilas (Matriz a) = a

-- Retorna matríz de ceros de n renglones y m columnas                                                                                                 
ceroMatriz :: Int -> Int -> Matriz Double
ceroMatriz n m = Matriz (ceroaux n m)

ceroaux :: Int -> Int -> [Vector Double]
ceroaux 0 _ = []
ceroaux n m = (Vector (take m [0.0,0.0..])):ceroaux (n-1) m

-- *** -- *** -- *** -- *** -- *** -- *** -- *** -- *** -- ***

v1 :: Vector Double
v1 = Vector [1,1,1,1]
v2 :: Vector Double
v2 = Vector [5.1,4,0.5,3]
v3 :: Vector Double
v3 = Vector [0,0,1]
m1 :: Matriz Double
m1 = Matriz [Vector [1,1,1], v3, Vector [0,1,1.5]]
m2 :: Matriz Double
m2 = Matriz [Vector [1,1,1], Vector[2,2,2], Vector[4,4,4]]