"la segunda asignaci�n de programaci�n requerir� que escriba 
una funci�n R que pueda almacenar en cach� los c�lculos que 
consumen mucho tiempo. Por ejemplo, tomar la media de un 
vector num�rico suele ser una operaci�n r�pida. Sin embargo,
para un vector muy largo, puede tomar demasiado tiempo calcular 
la media, especialmente si tiene que calcularse repetidamente
(por ejemplo, en un bucle). Si el contenido de un vector no 
est� cambiando, puede tener sentido almacenar en cach� el valor 
de la media para que cuando lo necesitemos de nuevo, se pueda 
buscar en la memoria cach� en lugar de volver a calcular. En esta
asignaci�n de programaci�n aprovechar� las reglas de alcance del
lenguaje R y c�mo se pueden manipular para preservar el estado 
dentro de un objeto R."
  
"Criterios de revisi�n"

"Esta tarea se calificar� mediante evaluaci�n por pares. Durante 
la fase de evaluaci�n, debe evaluar y calificar las presentaciones
de al menos 4 de sus compa�eros de clase. Si no completa al menos 4 
evaluaciones, su calificaci�n de asignaci�n se reducir� en un 20%."


"Ejemplo: almacenamiento en cach� de la media de un vector"

"En este ejemplo, presentamos el operador << - que se puede utilizar
para asignar un valor a un objeto en un entorno que es diferente del 
entorno actual. A continuaci�n hay dos funciones que se utilizan para
crear un objeto especial que almacena un vector num�rico y la media
de la memoria cach�.

La primera funci�n, makeVector crea un 'vector' especial, que es 
realmente una lista que contiene una funci�n para

establecer el valor del vector
obtener el valor del vector
establecer el valor de la media
obtener el valor de la media"



makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

"La siguiente funci�n calcula la media del 'vector' especial creado 
con la funci�n anterior. Sin embargo, primero verifica si la media ya 
se ha calculado. Si es as�, obtiene la media del cach� y omite el c�lculo.
De lo contrario, calcula la media de los datos y establece el valor de la
media en la memoria cach� mediante la funci�n setmean."


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

"Asignaci�n: almacenamiento en cach� de la inversa de una matriz"

"La inversi�n en matriz suele ser un c�lculo costoso y puede haber alg�n 
beneficio en el almacenamiento en cach� de la inversa de una matriz en lugar
de calcularla repetidamente (tambi�n hay alternativas a la inversi�n en matriz
que no discutiremos aqu�). Su tarea es escribir un par de funciones que
almacenan en cach� el inverso de una matriz.

Escribe las siguientes funciones:

makeCacheMatrix: esta funci�n crea un objeto especial 'matriz' que puede 
almacenar en cach� su inverso.
cacheSolve: Esta funci�n calcula el inverso de la 'matriz' especial devuelta
por makeCacheMatrix arriba. Si ya se ha calculado el inverso (y la matriz no 
ha cambiado), entonces el cach� debe recuperar el inverso del cach�.
Calcular el inverso de una matriz cuadrada se puede hacer con la funci�n 
resolver en R. Por ejemplo, si X es una matriz invertible cuadrada,
resolver (X) devuelve su inverso.

Para esta asignaci�n, suponga que la matriz suministrada siempre es invertible."





