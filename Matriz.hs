-- Recrdemos que se podrá trabajar con matrices de a lo más dimensión 5x5
module Matriz where
import EspacioVectorial
import Data.List
import Data.Ord (comparing)

--Funcion que transforman una matriz en una lista de listas
matrizLista :: Matriz a -> [[a]]
matrizLista (Matriz l) = map (\x -> getVector x) l

--Función que transforma una lista de listas en matriz
listaMatriz :: [[a]] -> Matriz a
listaMatriz as = Matriz (map (\x -> (Vector x)) as)

--Función para obtener la transpuesta de una matríz
transpuesta :: (Fractional a) => Matriz a -> Matriz a
transpuesta = listaMatriz . auxTrans . matrizLista

auxTrans :: [[a]] -> [[a]]
auxTrans ([]:_) = []
auxTrans xs = (map head xs): (auxTrans (map (\x -> drop 1 x) xs))

-- Comenzando desde el índice 1 la entrada (1,1) es la primera entrada
-- n simboliza el renglón y m la columna en donde se debe buscar el valor
getValue :: (Num a) => (Int,Int) -> Matriz a -> a
getValue (n,m) (Matriz l) = let row = getVector (l !! (n-1)) in (row !! (m-1))

{--Se implementa el conjunto de operaciones elementales para matrices --}
-- Función que multiplica un renglon de la maríz por un escalar.
-- El entero simboliza la fila que se desea multiplicar, la función también recibe el escalar y la matriz
multFila :: (Fractional a) => Int -> a -> Matriz a -> Matriz a
multFila f e (Matriz l) = let v = multiplica e (l !! (f-1)) in Matriz ((take (f-1) l) ++ [v] ++ (drop f l))

-- Intercambia dos filas de la matríz de lugar
swapFila :: Int -> Int -> Matriz a -> Matriz a
swapFila f1 f2 (Matriz a) = let v1 = (a !! (f1-1))
                                v2 = (a !! (f2-1))
                                left = take (f1-1) a
                                mid = take (f2-f1-1) (drop f1 a)
                                right = drop f2 a
                                in Matriz (left ++ [v2] ++ mid ++ [v1] ++ right)

-- Función que suma un renglón a otro
-- f1 y f2 representan las filas a sumar, el resultado se insertará en f2, es decir en el segundo argumento                       
sumaFila :: (Fractional a) => Int -> Int -> Matriz a -> Matriz a
sumaFila f1 f2 (Matriz m) = let sum = suma (m !! (f1-1)) (m !! (f2-1))
                                left = take (f2-1) m
                                right = drop f2 m
                                in Matriz (left ++ [sum] ++ right)

-- El primer tipo "a" corresponde al escalar que se multiplicará por la fila 1 (f1) para a continusación sumarla con f2
multSumaFila :: (Fractional a) => a -> Int -> Int -> Matriz a -> Matriz a
multSumaFila esca f1 f2 m = let x = multFila f1 esca m
                                in multFila f1 (1/esca) $ sumaFila f1 f2 x

-- Devuelve una tupla que simboliza el tamaño de la matriz en filas y columnas (fila,columna)
sizeM :: Matriz a -> (Int,Int)
sizeM (Matriz a) = let rows = length a
                       cols = length $ getVector $ head a 
                       in (rows,cols)

-- Toma n filas de una matriz
takeFila :: Int -> Matriz a -> Matriz a
takeFila n (Matriz a) = Matriz (take n a)

-- selecciona la fila n de la matriz
selectFila :: Int -> Matriz a -> [a]
selectFila n (Matriz a) = getVector $ a !! (n-1)

-- A partir de aquí trabajamos únicamente con matrices cuadradas y aumentadas
-- Sirve para dejar únicamente la matriz en forma diagonal con ayuda de las operaciones elementales 
gaussJordan :: (Eq a, Fractional a) => Matriz a -> Matriz a
gaussJordan m = escalona ren ren m where ren = fst $ sizeM m

-- Escalona una matriz, pone valores nulos en todas las posiciones salvo en la diagonal y vuelve 1 la diagonal
escalona :: (Eq a, Fractional a) => Int -> Int -> Matriz a -> Matriz a
escalona 0 0 mat = mat
escalona ren col mat = if (getValue (ren,col) mat) == 1 then escalona (ren-1) (col-1) (fase2 (delete ren [1..t]) col mat) else escalona ren col $ (fase1 (ren,col) mat)
                        where t = fst $ sizeM mat

--fase 2 vulve cero las posiciones que no se hayan en la diagonal, diagonaliza la matríz
-- la lista representa a las filas distintas a la fila c (las que deben ser modificadas) y m a la matriz
fase2 :: (Fractional a) => [Int] -> Int -> Matriz a -> Matriz a
fase2 [] _ m = m
fase2 (x:xs) c m = let val = getValue (x,c) m
                       in fase2 xs c $ multSumaFila (- val) c x m

-- fase 1 vuelve 1 la entrada (n,n) y los demás elementos de la fila quedan modificados
fase1 :: (Fractional a) => (Int,Int) -> Matriz a -> Matriz a
fase1 (x,y) m = let v = getValue (x,y) m
                    in multFila x (1/v) m

-- En caso que sea posible acomoda en la diagonal de la matriz valores no nulos
diagonalNoNula :: (Eq a, Num a) => Matriz a -> Matriz a
diagonalNoNula m = let tam = sizeM m
                       ren = fst tam
                       in swapDiagonal (candidates ren ren m []) m (matrizLista m)

-- Nos brinda una lista de parejas en la que la primer posición de cada tupla corresponde a una columna de la matriz
-- y la lista que lo acompaña representa las posibles filas que pueden intersectar con dicha columna para que la diagonal
-- no posea elementos nulos, se utiliza recursión de cola
-- tam = número de filas en la matriz; ren = fila en la que nos encontramos parados en las llamadas recursivas;
-- m la matriz original; x = lista en la que se van añadiendo las parejas antes descritas.
candidates :: (Eq a, Num a) => Int -> Int -> Matriz a -> [(Int,[Int])] -> [(Int,[Int])]
candidates _ 0 _ x = (sortBy $ comparing (length . snd)) x
candidates tam ren m x = candidates tam (ren-1) m (x ++ [(ren, [y | y<-[1..tam], (getValue (y,ren) m) /= 0])])

-- Dada la lista de la función anterior intercabia las filas de la matriz para que en la diagonal obtengamos 
-- elementos no nulos, en caso que esto no sea posible se lanza un error, utilizamos recursión de cola.
-- Recibimos la lista de la función anterior, la matríz y la lista de vectores de la matriz
swapDiagonal :: [(Int,[Int])] -> Matriz a -> [[a]] -> Matriz a
swapDiagonal [] m list = listaMatriz list
swapDiagonal ((a,as):xs) m list = if length as /= 0 
                                  then let elem = head as
                                           (y,_:ys) = splitAt (a-1) list
                                           in swapDiagonal (map (\(a,c) -> (a,delete elem c)) xs) m (y ++ [selectFila elem m] ++ ys)
                                  else error "Sistema sin solución"

--}
--determinante
m3 :: Matriz Double                    
m3 = Matriz [Vector [1,0,0], Vector [0,2,0], Vector [0,0,4]]
