  # Practico 2: Modelos lineales

La idea de este práctico es poner en práctica elementos del módulo 1 en el que vimos funciones y procesos iterados, con elementos del módulo 2 en el que vimos regresión simple, multiple y tuvimos una breve introducción a regresión logística y modelos panel.

El practico lo desarrollare con la misma base que utilizamos durante la clase pero puedes usar cualquier otra.

## Ejercicio 1:

Los modelos de regresión deben cumnplir con ciertos supuestos para garantizar que los estimadores obtenidos son los "MELI" (Mejor Estimador Lineal Insesgado) son cuatro supuestos respecto a los errores y dos supuestos adicionales, uno con respecto a la no multicolinealidad y otro con respecto a la existencia de valores  extremos.

Crea una función que reciba un modelo y verifique los supuestos respecto a los errores, el resultado debe ser una tabla  con el resumen de las pruebas, existen varios paquetes que permiten hacer eso, yo te dejo las funciones que yo suelo usar.

### Normalidad

`tseries::jarque.bera.test()`

### Media cero

`t.test(x = , mu = 0)`

### Homocedasticidad (varianza constante)

`lmtest::bptest()`

### No autocorrelacion

`lmtest::dwtest()`

## Ejercicio 2:

En clase vimos como correr el mismo modelo (la misma especificacion de variables dependiente e independiente) para distintas bases de datos de manera iterada. Animate a generar un proceso  iterado en el que apliques distintas especificaciones de modelos sobre la misma base de datos.
Te copio la función y el proceso iterado que utilizamos para el mismo modelo en distintas bases

    regresion_mul <- function (df,         
                               var_dep,    
                               vars) {
      
    # 1. Correr la regresión con lm()
    
    p <- lm(formula = df[[var_dep]] ~ .,
            data = df[,vars])
    
    # 2. Guardar el summary de la regresion
    
    q <- summary(p)
    
    # 3. Crear tabla resumen
    
    coef <- tibble(year = unique(df$date),                                       # Almacenamos año de la base
                   Nombres = rownames(q$coefficients[vars, ]),                   # Nombres de variables independientes
                   Estimacion = round(q$coefficients[vars, 1], 4),               # Valor de coeficientes independientes
                   p.Valor = round(q$coefficients[vars, 4], 4),                  # P-VAlor de variables independientes
                   R2 = q$r.squared) %>%                                         # R cuadrado de la regresión
      mutate(Significatividad = ifelse(test = p.Valor < 0.05,                    # Variable que resume la significatividad de cada variable independiente
                                       yes = "Significativo",
                                       no = "No Significativo"))
    
    return(coef)                                                                 # Retorna tabla
    }


    # Prueba de la función:

    # Vector con variables independientes

    variables <- c("trade", "govSpending", "Inflation")

    # Aplicacion

    regresion_mul(df = base_year[[1]], 
                  var_dep = "growthGdp", 
                  vars = variables)

    # Iteracion de función en todas las bases
    
    reg_mul_years <- map_dfr(.x = base_year,                             # lista con elementos a iterar
                             .f = ~{
                           
                                   regresion_mul(df = .x,                    # .x porque es el df lo que va a irse modificando
                                                 var_dep = "growthGdp",      # Variable dependiente fija
                                                 vars = variables)           # variables independientes en vector 'variables'
                                   
                                 })


## Ejercicio 3:

Existe un paper maravillos de Bertrand & Mullainathan en el que buscan generar evidencia sobre la discriminacion racial en el ambito laboral. Generaron CV falsos pero con caracteristicas que permitian al empleador darse cuenta si el postulante era Negro o Blanco. Por suerte dejaron toda esa información a disposición para replicar el análisis. Usando la base 'resume.csv', estima dos modelos logisticos. La variable dependiente es 'received_callback'.

1. El primer modelo usando como variables independientes `years_experience`, `years_college` y `race`.
2. El segundo modelo usando como variables independientes `job_city`, `years_experience`, `honors` y `race`.

Cual es mejor según AIC?
