-- Recrdemos que se podrá trabajar con matrices de a lo más dimensión 5x5
module Matriz where
import EspacioVectorial

--Funcion que transforman una matriz en una lista de listas
matrizLista :: (Matriz a) -> [[a]]
matrizLista (Matriz l) = map (\x -> getVector x) l

--Función que transforma una lista de listas en matriz
listaMatriz :: [[a]] -> (Matriz a)
listaMatriz as = Matriz (map (\x -> (Vector x)) as)

--Función para obtener la transpuesta de una matríz
transpuesta :: (Num a) => (Matriz a) -> (Matriz a)
transpuesta = listaMatriz . auxTrans . matrizLista

auxTrans :: [[a]] -> [[a]]
auxTrans ([]:_) = []
auxTrans xs = (map head xs): (auxTrans (map (\x -> drop 1 x) xs))

-- Comenzando desde el índice 1 la entrada (1,1) es la primera entrada
-- n simboliza el renglón y m la columna en donde se debe buscar el valor
getValue :: (Num a) => Int -> Int -> (Matriz a) -> a
getValue n m (Matriz l) = let row = getVector (l !! (n-1)) in (row !! (m-1))

{--Se implementa el conjunto de operaciones elementales para matrices --}
-- Función que multiplica un renglon de la maríz por un escalar.
-- El entero simboliza la fila que se desea multiplicar, la función también recibe el escalar y la matriz
multFila :: (Num a) => Int -> a -> (Matriz a) -> (Matriz a)
multFila f e (Matriz l) = let v = multiplica e (l !! (f-1)) in Matriz ((take (f-1) l) ++ [v] ++ (drop f l))

-- Intercambia dos filas de la matríz de lugar
swapFila :: Int -> Int -> (Matriz a) -> (Matriz a)
swapFila f1 f2 (Matriz a) = let v1 = (a !! (f1-1))
                                v2 = (a !! (f2-1))
                                left = take (f1-1) a
                                mid = take (f2-f1-1) (drop f1 a)
                                right = drop f2 a
                                in Matriz (left ++ [v2] ++ mid ++ [v1] ++ right)

-- Función que suma un renglón a otro
-- f1 y f2 representan las filas a sumar, el resultado se insertará en f2, es decir en el segundo argumento                       
sumaFila :: (Num a) => Int -> Int -> (Matriz a) -> (Matriz a)
sumaFila f1 f2 (Matriz m) = let sum = suma (m !! (f1-1)) (m !! (f2-1))
                                left = take (f2-1) m
                                right = drop f2 m
                                in Matriz (left ++ [sum] ++ right)

gaussJordan :: (Num a) => (Matriz a) -> (Matriz a)
gauusJordan (Matriz (m:ms)) = 