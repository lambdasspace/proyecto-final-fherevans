
module EspacioVectorial where

-- Creamos una clase de tipos para todos los elementos que puedan ser parte de un espacio vectorial
-- Todo espacio vectorial debe implementar la operación de suma de vectores y multiplicación por escalares.
-- Un EV tiene un elemento cero, hará falta especificar la dimensión del EV para obtener dicho vector 
    class EV a where
        suma :: (Num e) => (a e) -> (a e) -> (a e)
        multiplica :: (Num e) => e -> (a e) -> (a e)

-- tipo para las n-adas
    data Vector a = Vector [a] deriving Show

-- definimos Vector como instancia de la clase EV
    instance EV Vector where
        suma (Vector x) (Vector y) = if verifica x y then Vector (zipWith (+) x y) else error "Solo se pueden sumar vectores del mismo espacio vectorial"
        multiplica k x = fmap (*k) x

    instance Functor Vector where
        fmap f (Vector x) = Vector (map f x)
    

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
    ceroVector :: (Integral a) => a -> Vector a
    ceroVector n = fmap (*0) (Vector [1..n])

-- *** -- *** -- ***
-- Tipo de dato matríz, puede definir una materíz por fila o una matríz por columna
    data Matriz a = Matriz [Vector a] deriving Show

-- Instanciamos a matriz como un espacio vectorial
    instance EV Matriz where
        suma (Matriz xs) (Matriz ys) = if verificaSum xs ys then let p = zip xs ys 
                                                                 in Matriz (map (\(a,b) -> suma a b) p)
                                       else error "No es posible sumar matrices de distintos EV"
        multiplica a (Matriz xs) = Matriz (map (\x -> multiplica a x) xs)

-- Función auxiliar con la cuál verificamos si un par de matrices tienen la misma dimensión
    verificaSum :: [Vector a] -> [Vector a] -> Bool 
    verificaSum xs ys = if (length xs) == (length ys) then let p = zip xs ys in
                                                               and $ map (\(a,b) -> (length $ getVector a)==(length $ getVector b)) p
                                                    else False

-- Retorna matríz de ceros de n renglones y m columnas                                                                                                 
    ceroMatriz :: (Integral a) => a -> a -> Matriz a
    ceroMatriz n m = Matriz (ceroaux n m)

    ceroaux :: (Integral a) => a -> a -> [Vector a]
    ceroaux 0 m = []
    ceroaux n m = (fmap (*0) (Vector [1..m])):ceroaux (n-1) m

-- *** -- *** -- ***
-- Tipo de dato polinomio que hereda de EV
-- La posición de cada elemento de la lista corresponde al grado de la literal que acompaña coeficiente
-- Por ejemplo el polinomio "3 + 2x^2 + x^3" equivale a: "Polinomio [3,0,2,1]"
    data Polinomio a = Polinomio [a] deriving Show

-- Instanciamos a nuestro polinomio como miembre de espacio vectorial
    instance EV Polinomio where
        suma ps qs = Polinomio (auxSuma (getPol ps) (getPol qs))
        multiplica c (Polinomio p1) = Polinomio (map (*c) p1)

-- Retorna la lista que corresponde a los valores del polinomio
    getPol:: Polinomio a -> [a]
    getPol (Polinomio x) = x

{-- Auxiliar para poder realizar la suma, recordemos que el grado de la suma de dos polinomios
    es igual al grado del polinomio de mayor grado de los sumandos--}
    auxSuma :: (Num a) => [a] -> [a] -> [a]
    auxSuma [] [] = []
    auxSuma p1 [] = p1
    auxSuma [] p2 = p2
    auxSuma (p:ps) (q:qs) = p+q:auxSuma ps qs

-- cero para los polinomios, es igual al cero de los vectores, solamente hará falta indicar la dimensión del EV
    ceroPol :: (Integral a) => a -> Polinomio a
    ceroPol n = Polinomio (map (*0) [1..n])
   