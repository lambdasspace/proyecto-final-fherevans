# Implementado hasta el momento

1. Los espacios vectoriales de **Matrices**, **Vectores**
Polinomios puede verse como un caso de los vectores

  * suma de vectores
  * multiplicación por escalares
  * el vector cero de cada espacio
  * fmap para para matriz y vector
  * función *show* para matriz y vector
  * funciones que lista de vectores a matrices
  * producto punto de vectores

2. Operaciones para matrices
  * función transpuesta
  * recuperación de un valor en determinadas coordenadas de la matriz
  * operaciones elementales para matrices
    + intercambio de renglones
    + suma de renglones
    + multiplicación de una fila por un escalar
    + una función que realiza la multiplicación de un renglón y lo suma a otro
  * función para diagonalizar
  * algoritmo de Gauss Jordan que resuleve sistemas de ecuaciones lineales
  * funciones que brindan información de matrices (tamaño, tomar filas, tomar un valor en cierta coordenada)

3. Bases y dimensión
  * Función que determina si un conjunto de vectores de dimensión a lo más 4 es LI
    + Si el conjunto tiene el mismo tamaño que la dimensión de los vectores puede determinar si es LI para un ev de cualquier dimensión
    + También para cuales quiera 2 vectores, sin importar la dimensión del ev al que pertenecen se puede determinar si son LI
  * Función LD que se sirve de LI para saber si un conjunto es LD
  * Función que determina si hay un cero vector en el conjunto o matriz
  * Función que nos otorga la base canónica de un espacio de dimensión n
  * Función que nos dice si un conjunto de vectores es base del EV de a lo más dimensión 4

4. Se añade el main, pequeño programa interactivo para probar las principales funcionalidades del sistema