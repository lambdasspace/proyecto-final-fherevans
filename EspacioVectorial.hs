
module EspacioVectorial where

-- Creamos una clase de tipos para todos los elementos que puedan ser parte de un espacio vectorial
-- Todo espacio vectorial debe implementar la operación de suma de vectores y multiplicación por escalares.
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

-- Tipo de dato polinomio que hereda de EV
    data Polinomio a = Polinomio Char [a] deriving Show
        suma (Polinomio x p1) (Polinomio x p2) = Polinomio x (map (\(a,b) -> a+b) (zip p1 p2))
        multiplica p1 p2 = Polinomio


    verificaP :: [a] -> [a] -> Bool
    verificaP l1 l2 = (length l1) < 6 && (length l2) < 6

   