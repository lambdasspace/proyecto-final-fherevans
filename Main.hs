module Main where
import EspacioVectorial
import System.IO
import Matriz

--Función para correr el main
run :: IO ()
run = main

-- Función que ejecuta el programa interactivo
main :: IO ()
main = do 
    c <- muestraMenu
    procesa c
    if c == '0' then
        putStr "Saliendo del programa\n"
    else
        main

-- Función que imprime el menu y recolecta la acción que el usuario desea realizar
muestraMenu :: IO Char
muestraMenu = do putStrLn "\n**BIENVENIDO AL PROGRAMA INTERACTIVO PARA LA BIBLIOTECA DE ÁLGEBRA LINEAL**"
                 putStrLn "Indica la acción que deseas realizar\n"
                 putStrLn "[0] Salir del programa"
                 putStrLn "[1] Calcular el producto punto de dos vectores"
                 putStrLn "[2] Obtener la transpuesta de una matriz"
                 putStrLn "[3] Realizar operaciones elementales con matrices"
                 putStrLn "[4] Hallar la solución a un sistema de ecuaciones"
                 putStrLn "[5] Determinar si un conjunto de vectores es L.I."
                 putStrLn "[6] Determinar si un conjunto de vectores es L.D."
                 putStrLn "[7] Obtener una base canónica"
                 putStrLn "[8] Determinar si un conjunto es base de un espcio"
                 putStr "\n Tu elección: "
                 opcion <- getChar
                 return opcion

-- Función que procesa la elección del usuario

procesa :: Char -> IO ()
procesa '1' = prodPunto
procesa '2' = transpon
procesa '3' = menuOperaciones
procesa '4' = sistemaEcuaciones
procesa '5' = esLi
procesa '6' = esLd
procesa '7' = canonica
procesa '8' = bases
procesa '0' = putStrLn "\nbye"
procesa _ = putStrLn "\nOpción inválida\n"

-- Nos solicita la información de los vectores a los que les sacaremos el producto punto e imprime el resultado
prodPunto :: IO ()
prodPunto = do putStrLn "\n¿Cuántos elementos tienen los vectores que vas a operar?"
               res <- getLine
               putStrLn "\n--Dame el primer vector\n"
               l1 <- (askVector (read res::Int) [])
               putStrLn $ "v1 = "++ show l1
               putStrLn "\n--Dame el segundo vector\n"
               l2 <- (askVector (read res::Int) [])
               putStrLn $ "v2 = "++ show l2
               putStrLn $ "\nEl producto interno de <"++(show l1)++","++(show l2)++"> es : "++ (show $ productoPunto (Vector l1) (Vector l2))
               
--Le pide un vector al usuario
askVector :: Int -> [Double] -> IO [Double]
askVector 0 l = return l
askVector x l = do putStr "Introduce un valor: "
                   input <- getLine
                   askVector (x-1) (l++[read input :: Double])

-- Función que le pide una matriz al usuario y devuelve su transpuesta
transpon :: IO ()
transpon = do l1 <- pideMatriz
              putStrLn $ "La matriz que introduciste es: \n" ++ (show $ listaMatriz l1)
              putStrLn $ "Su transpuesta es: \n" ++ (show $ transpuesta (listaMatriz l1))

--Solicita una matriz al usuario
pideMatriz :: IO [[Double]]
pideMatriz = do putStrLn "\n¿Cuántas filas tiene tu matriz?"
                f <- getLine
                putStrLn "\n¿Cuántas columnas?"
                c <- getLine
                putStrLn "\n--Dame los vectores fila \n"
                askMatriz 1 (read f::Int) (read c::Int) []

-- Le pide al usuario que introduzca una matriz dado el tamaño que este define en un inicio
-- Sus renglones y columnas
askMatriz :: Int -> Int -> Int -> [[Double]] -> IO [[Double]]
askMatriz _ 0 _ l = return l
askMatriz e fila col l = do putStr $ "Dame la fila " ++ (show e) ++ "\n"
                            v <- askVector col []
                            putStr "\n"
                            askMatriz (e+1) (fila-1) col (l++[v])

--Despliega las opciones disponibles para operar con matrices
menuOperaciones :: IO ()
menuOperaciones = do putStrLn "\n*Aquí puedes realizar alguna operación elemental con tu matriz*\n"
                     putStrLn "Introduce la matriz con la que vas a trabajar: \n"
                     l1 <- pideMatriz
                     putStrLn $ "\nLa matriz que introduciste es: \n" ++ (show $ listaMatriz l1) ++ "\n"
                     putStrLn "¿Qué operación deseas hacer?\n"
                     putStrLn "[a] Sumar dos filas"
                     putStrLn "[b] Intercambiar filas"
                     putStrLn "[c] Multiplicar un escalar por una fila"
                     putStrLn "[d] Realizar una multiplicación y sumar el resultado a una fila"
                     putStrLn "[x] Salir"
                     putStr "Tu respuesta: "
                     respuesta <- getChar
                     realizaOperacion l1 respuesta

--Caza de patrones para realizar la acción que el usuario desee sobre una matriz
--Responde a operaciones básicas sobre sistemas de ecuaciones
realizaOperacion :: [[Double]] -> Char -> IO ()
realizaOperacion l1 'a' = do putStr "\nSelecciona una fila: "
                             f1 <- getLine
                             putStr "Selecciona la fila a la que se la sumarás: "
                             f2 <- getLine
                             putStrLn $ "\nLa matriz resultante es: \n" ++ (show $ sumaFila (read f1::Int) (read f2::Int) (listaMatriz l1))
realizaOperacion l1 'b' = do putStr "\nSelecciona una fila: "
                             f1 <- getLine
                             putStr "Selecciona la fila a la que se la sumarás: "
                             f2 <- getLine
                             putStrLn $ "\nLa matriz resultante es: \n" ++ (show $ swapFila (read f1::Int) (read f2::Int) (listaMatriz l1))
realizaOperacion l1 'c' = do putStr "\nSelecciona una fila: "
                             f1 <- getLine
                             putStr "Por cuánto se multiplicará la fila: : "
                             c <- getLine
                             putStrLn $ "\nLa matriz resultante es: \n" ++ (show $ multFila (read f1::Int) (read c::Double) (listaMatriz l1))
realizaOperacion l1 'd' = do putStr "\nSelecciona una fila: "
                             f1 <- getLine
                             putStr "Por cuánto se multiplicará esta fila: "
                             c <- getLine
                             putStr "A qué fila le sumaremos el resultado: "
                             f2 <- getLine
                             putStrLn $ "\nLa matriz resultante es: \n" ++ (show $ multSumaFila (read c::Double) (read f1::Int) (read f2::Int) (listaMatriz l1))
realizaOperacion _ 'x' = main
realizaOperacion _ _ = do putStrLn "Opción no válida"

-- Dado un conjunto de ecuaciones regresa la solución del sistema de ecuaciones
sistemaEcuaciones :: IO ()
sistemaEcuaciones = do putStrLn "\nUna ecuación lineal será representada por una expresión y un resultado:\n"
                       putStrLn "Por ejemplo en:  \n a+b+c=1 \nLa expresión es a+b+c y el resultado 1"
                       putStrLn "\nDime cuántas variables distintas tienen las ecuaciones de tu sistema: "
                       res <- getLine
                       putStrLn $ "\nIntroduce " ++ res ++ " ecuaciones:\n"
                       mat <- construyeSist (read res::Int) (read res::Int) []
                       putStrLn $ "\nLas soluciones del sistema son: " ++ solucion (gaussJordan (listaMatriz mat))

-- Imprime una lista de tuplas de la forma (variable, resultado)
solucion :: (Show a) => Matriz a -> String
solucion mat = let s = sizeM mat
                   t = selectFila (snd s) (transpuesta mat)
                   l = (take (fst s) ['a','b'..])
                   in show (zip l t)


--Sirve para solicitarle al usuario una ecuación con su solución             
pideEcuacion :: [Char] -> [Double] -> IO [Double]
pideEcuacion [] l = do putStr "Dame el resultado de la ecuación: "
                       res <- getLine
                       return $ l++[read res::Double]
pideEcuacion (x:xs) l = do putStr $ "Dame el valor de "++ [x] ++ ": "
                           elem <- getLine
                           pideEcuacion xs (l++[read elem::Double])

--Construye el sistema de ecuanciones lineales
construyeSist :: Int -> Int -> [[Double]] -> IO [[Double]]
construyeSist _ 0 l = return l
construyeSist e n l = do putStrLn "Introduce los coeficientes que te solicitan a continuación:\n"
                         putStrLn $ "Ecuación " ++ show (e-(n-1)) ++ "\n"
                         v <- pideEcuacion (take e ['a','b'..]) []
                         construyeSist e (n-1) (l++[v])

--Función que pide al usuario un conjunto de vectores y verifica si estos son linealmente
-- independientes o no
esLi :: IO ()
esLi =  do l1 <- pideMatriz
           putStrLn $ "El conjunto que introduciste es: \n" ++ (show $ listaMatriz l1)
           if (li (sizeM (listaMatriz l1)) (listaMatriz l1)) then
               putStrLn "\nEl conjunto es LI"
            else
                putStrLn "\nEl conjunto no es LI"

--Realiza la acción contraria a la función anterior
esLd :: IO ()
esLd =  do l1 <- pideMatriz
           putStrLn $ "El conjunto que introduciste es: \n" ++ (show $ listaMatriz l1)
           if (ld (listaMatriz l1)) then
               putStrLn "\nEl conjunto es LD"
            else
                putStrLn "\nEl conjunto no es LD"

--Regresa una base canónica de un espacio de dimensión n
canonica :: IO ()
canonica = do putStrLn "\n¿De qué dimensión es el subespacio?"
              t <- getLine
              putStrLn $ "\nLa base canónica del espacio es:\n" ++ (show $ baseCanonica (read t::Int) (read t::Int))

--Verifica que el conjunto sea base de un EV
bases :: IO ()
bases = do l1 <- pideMatriz
           putStrLn $ "\nEl conjunto que introduciste es: \n" ++ (show $ listaMatriz l1)
           if (esBase (length $ head l1) l1) then
               putStrLn "\nEl conjunto es base"
            else
                putStrLn "\nEl conjunto no es base"

           

           