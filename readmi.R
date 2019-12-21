"la segunda asignación de programación requerirá que escriba 
una función R que pueda almacenar en caché los cálculos que 
consumen mucho tiempo. Por ejemplo, tomar la media de un 
vector numérico suele ser una operación rápida. Sin embargo,
para un vector muy largo, puede tomar demasiado tiempo calcular 
la media, especialmente si tiene que calcularse repetidamente
(por ejemplo, en un bucle). Si el contenido de un vector no 
está cambiando, puede tener sentido almacenar en caché el valor 
de la media para que cuando lo necesitemos de nuevo, se pueda 
buscar en la memoria caché en lugar de volver a calcular. En esta
asignación de programación aprovechará las reglas de alcance del
lenguaje R y cómo se pueden manipular para preservar el estado 
dentro de un objeto R."
  
"Criterios de revisión"

"Esta tarea se calificará mediante evaluación por pares. Durante 
la fase de evaluación, debe evaluar y calificar las presentaciones
de al menos 4 de sus compañeros de clase. Si no completa al menos 4 
evaluaciones, su calificación de asignación se reducirá en un 20%."


"Ejemplo: almacenamiento en caché de la media de un vector"

"En este ejemplo, presentamos el operador << - que se puede utilizar
para asignar un valor a un objeto en un entorno que es diferente del 
entorno actual. A continuación hay dos funciones que se utilizan para
crear un objeto especial que almacena un vector numérico y la media
de la memoria caché.

La primera función, makeVector crea un 'vector' especial, que es 
realmente una lista que contiene una función para

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

"La siguiente función calcula la media del 'vector' especial creado 
con la función anterior. Sin embargo, primero verifica si la media ya 
se ha calculado. Si es así, obtiene la media del caché y omite el cálculo.
De lo contrario, calcula la media de los datos y establece el valor de la
media en la memoria caché mediante la función setmean."


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

"Asignación: almacenamiento en caché de la inversa de una matriz"

"La inversión en matriz suele ser un cálculo costoso y puede haber algún 
beneficio en el almacenamiento en caché de la inversa de una matriz en lugar
de calcularla repetidamente (también hay alternativas a la inversión en matriz
que no discutiremos aquí). Su tarea es escribir un par de funciones que
almacenan en caché el inverso de una matriz.

Escribe las siguientes funciones:

makeCacheMatrix: esta función crea un objeto especial 'matriz' que puede 
almacenar en caché su inverso.
cacheSolve: Esta función calcula el inverso de la 'matriz' especial devuelta
por makeCacheMatrix arriba. Si ya se ha calculado el inverso (y la matriz no 
ha cambiado), entonces el caché debe recuperar el inverso del caché.
Calcular el inverso de una matriz cuadrada se puede hacer con la función 
resolver en R. Por ejemplo, si X es una matriz invertible cuadrada,
resolver (X) devuelve su inverso.

Para esta asignación, suponga que la matriz suministrada siempre es invertible."





