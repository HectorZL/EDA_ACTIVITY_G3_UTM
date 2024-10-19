#                 Practica-2
#_______
#           Integrantes
#             Grupo #3
#
#• LOOR PAZMIÑO JANDRY JOSUE 
#• PALMA LAZ ARIEL LEONARDO
#• QUIMI RAMIREZ IVAN JOSUE 
#• ZAMBRANO LOOR HECTOR JESUS 
#
#            Carrera:
#
#     Sistemas de Informacion
#_______
# Crear un script R con las siguientes características:
#_______
#_______




#Instalar la libreria.
if (!require(tidyverse)) install.packages("tidyverse")
library(dplyr)

# Config -------------------------------------------------------------
# Para notación no numérica.
options(scipen=999) 
# Para reproducibilidad.
set.seed(2024) 


#________Actividad__________
#Seleccion:
#___________________
#Primero se cargan los datos dados por la empresa.
ventas_ecuador <- read.csv("Datos/ventas_ecuador.csv") #Se usa la funcion read, y con el formato que se quiere leer (csv)

#Se muestra una vista previa de los datos con la funcion glimpse()
glimpse(ventas_ecuador)


#___________________
#Limpieza:
#                     Eliminación de observaciones duplicadas
#___________________

# Verificamos si hay duplicados, a traves de la comparacion de todas las listas
ventas_repetidas <- ventas_ecuador |> 
  group_by_all() |> 
  filter(n() > 1) |> 
  ungroup()

# Vemos las repetidas
glimpse(ventas_repetidas)

#Se eliminan duplicados con el distinct()
ventas_ecuador_limpio <- ventas_ecuador |> distinct()

#Con la funcion cat, se mostrara en pantalla el numero de filas originales
cat("Filas originales:", nrow(ventas_ecuador),"\n")

#Con la funcion cat, se mostrara en pantalla el numero de filas repetidas
cat("Filas con repetidos:", nrow(ventas_repetidas), "\n")

#Con la funcion cat, se mostrara en pantalla el numero de filas eliminadas
cat("Filas después de eliminar duplicados:", nrow(ventas_ecuador_limpio), "\n")

#___________________
#                             Valores Faltantes
#___________________

# Verificamos los NA, variables que no tienen datos.
# Usamos el pipe native, concatenar la funcion y pasar como argumento a la funcion summarise_all().
# la funcion summarise_all(), se usa para aplicar la siguiente funcion a toda la columna.
# ~, es para crear una funcion lambda
valores_faltantes <- ventas_ecuador_limpio |> summarise_all(~sum(is.na(.)))

# Mostramos los Na en cada variable
glimpse(valores_faltantes)

#Se realiza esta funcion para correguir los NA.
ventas_ecuador_limpio <- ventas_ecuador_limpio |>
  mutate(
    # Reemplazamos los NA en 'edad_cliente' con la mediana de la columna
    # usamos el na.rm = TRUE, Para remover los NA antes de hacer cualquier calculo, sino saldra NA.
    edad_cliente = ifelse(
      #Condicion del ifelse
      is.na(edad_cliente), 
      #Si es verdadero se hace esto
      median(edad_cliente, na.rm = TRUE), 
      #Si es falso, se realiza esto (NO Hace nada)
      edad_cliente)
  )

#Verificamos como afecta el NA, si no se pone el na.rm = TRUE,  (Sale NA)
hola <- ventas_ecuador_limpio |> 
  pull(edad_cliente) |> 
  mean()
glimpse(hola)

# Verificamos nuevamente si quedan NA en 'edad_cliente'
valores_faltantes <- ventas_ecuador_limpio |>
  summarise(edad_cliente = sum(is.na(edad_cliente)))

glimpse(valores_faltantes)

#___________________
#                         ERRORES ESTRUCTURALES
#___________________

# Convertimos todos los nombre a formato titulo con str_to_title
ventas_ecuador_limpio <- ventas_ecuador_limpio |>
  mutate(ciudad = str_to_title(ciudad))
# Verificamos
ventas_ecuador_limpio |> 
  count(ciudad)



# Verificamos las formas de escritura de cada ciudad
ventas_ecuador_limpio |> 
  count(ciudad)
# Filtrar para eliminar fechas superiores al año 2024
ventas_ecuador_limpio <- ventas_ecuador_limpio |>
  filter(year(fecha) <= 2024)

# Imprimir el resultado
print(ventas_ecuador_limpio)

# Obtener el rango actual de fechas en el dataset
rango_fechas <- ventas_ecuador_limpio |>
  summarise(
    fecha_min = min(fecha),
    fecha_max = max(fecha)
  )
#Imprimir el rango de las dfechas
print(rango_fechas)

#___________________
#Enriquesimiento de datos:
#                             Enriquecimiento geográfico
#___________________


#Ahora vamos a agregar nuevas columnas, para eso haremos un enriquecimiento de datos.
#En este caso se agregara la region en la que se encuentra cada ciudad.

ventas_ecuador_enriquecido <- ventas_ecuador_limpio %>% 
  #Hay que recordar que mutate se usa para modificar una columna o agregar otra
  mutate(
    region = case_when( #El case_when, es una funcion que crea una columna con valores basado en condicinoes logicas, es similar al if-else
      #Se usara el %in%, el cual sirve para verificar si un elemento se encuentra en un vector o dataframe
      # su sintaxis es: elemento %in% vector
      # Ejemplo: 5 %in% c(1, 2, 3, 4, 5) se dice, si el 5 se encuentra en ese vector. como un filtro.
      ciudad %in% c("Quito") ~ "Sierra",
      ciudad %in% c("Guayaquil", "Manta", "Portoviejo") ~ "Costa",
      ciudad == "Cuenca" ~ "Austro",
      TRUE ~ "Otra"
    )
  )
glimpse(ventas_ecuador_enriquecido)

#___________________
#                             Enriquecimiento demografico
#___________________

ventas_ecuador_enriquecido <- ventas_ecuador_enriquecido |>
  mutate(
    #La funcion cut(), sirve para categorizar valores de la columna edad_grupo
    edad_grupo = cut(edad_cliente, 
                     #el breaks, define los distintos cortes o rango para cada categoria
                     breaks = c(0, 25, 40, 60, Inf),
                     #el labels son las etiqueta que tendra cada rango del breaks
                     labels = c("Joven", "Adulto joven", "Adulto", "Adulto mayor"),
                     #el right sirve para tomar el rango del intervalo derecho, si se coge el 25 o no.
                     right = FALSE)
  )
glimpse(ventas_ecuador_enriquecido)

#___________________
#                            Enriquecimiento de productos
#___________________
#Añadiremos una categorización de productos:
ventas_ecuador_enriquecido <- ventas_ecuador_enriquecido |>
  mutate(
    categoria_producto = case_when(
      producto %in% c("Arroz", "Atún", "Leche", "Pan") ~ "Alimentos básicos",
      producto %in% c("Frutas", "Verduras") ~ "Productos frescos",
      producto %in% c("Carne", "Pollo") ~ "Proteínas",
      TRUE ~ "Otro"
    )
  )
glimpse(ventas_ecuador_enriquecido)




#___________________
# 2.3 ANALISIS EXPLORATIO DE DATOS(EDA)
#                       
#___________________

# Asegurémonos de que estamos trabajando con el conjunto de datos de calidad
ventas_ecuador_enriquecido <- ventas_ecuador_enriquecido |>
  mutate(fecha = as.Date(fecha))  # Asegurarse de que fecha es de tipo Date
glimpse(ventas_ecuador_enriquecido)



#TASA DEVOLUCION

# Resumen estadístico de la tasa de devolución
tasa_devolucion_resumen <- ventas_ecuador_enriquecido |>
  summarise(
    conteo = n(),
    media = mean(tasa_devolucion, na.rm = TRUE),
    mediana = median(tasa_devolucion, na.rm = TRUE),
    desviacion_estandar = sd(tasa_devolucion, na.rm = TRUE),
    min = min(tasa_devolucion, na.rm = TRUE),
    max = max(tasa_devolucion, na.rm = TRUE),
    q1 = quantile(tasa_devolucion, 0.25, na.rm = TRUE),
    q3 = quantile(tasa_devolucion, 0.75, na.rm = TRUE)
  )

print(tasa_devolucion_resumen)


# Histograma de la tasa de devolución
ggplot(ventas_ecuador_enriquecido, aes(x = tasa_devolucion)) +
  geom_histogram(bins = 10, fill = "pink", color = "black") +
  labs(title = "Distribución de la tasa de devolución",
       x = "Tasa de devolución", y = "Frecuencia")


# EDA REGION (CUALITATIVA)


#Análisis EDA de la región de manera cualitativa por región basándose en el código proporcionado:
#Frecuencia de ventas por región:
  
ventas_ecuador_enriquecido |>
  count(region)

#Visualización de ventas por región mediante un diagrama de barras:
ggplot(ventas_ecuador_enriquecido, aes(x = region, y = ventas)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Distribución de ventas (cantidad) por región",
       x = "Región", y = "Ventas (cantidad)")

ventas_ecuador_enriquecido |>
  group_by(region) |>
  summarise(total_ventas = sum(ventas)) |>
  ggplot(aes(x = reorder(region, -total_ventas), y = total_ventas)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Ventas totales ($) por región",
       x = "Región", y = "Ventas totales ($)")



# EDAD GRUPO()



ventas_ecuador_enriquecido %>%
  count(edad_grupo)

print(ventas_ecuador_enriquecido)

ggplot(ventas_ecuador_enriquecido, aes(x = edad_grupo, y = ventas)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribución de ventas (cantidad) por grupo de edad",
       x = "Grupo de edad", y = "Ventas (cantidad)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#EDAD RANGO


# Crear rangos de edad
ventas_ecuador_enriquecido$edad_rango <- cut(ventas_ecuador_enriquecido$edad_cliente, breaks = seq(0, 100, by = 10), right = FALSE)

# Gráfico de cajas por rango de edad
ggplot(ventas_ecuador_enriquecido, aes(x = edad_rango, y = tasa_devolucion, fill = edad_rango)) +
  geom_boxplot() +
  labs(title = "Distribución de la tasa de devolución por rango de edad del cliente",
       x = "Rango de Edad", y = "Tasa de Devolución") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  guides(color = guide_legend(title = "Rango de Edad")) +
  theme(legend.position = "bottom")


glimpse(ventas_ecuador_enriquecido)

#___________________
#Transformacion:
#                       Discretización de atributos
#___________________

# Discretizaremos la variable 'ventas' en categorías.
ventas_ecuador_transformado <- ventas_ecuador_enriquecido |>
  mutate(
    ventas_categoria = case_when(
      ventas < 150 ~ "Bajo",
      ventas >= 150 & ventas < 300 ~ "Medio",
      ventas >= 300 ~ "Alto"
    )
  )

#glimpse(ventas_ecuador_transformado)

# Categoria ventas


# CIUDAD NUMERICA Y PROMOCION NUMERICA

# Convertiremos algunas variables categóricas en numéricas:
ventas_ecuador_transformado <- ventas_ecuador_transformado |>
  mutate(
    ciudad_numerica = as.numeric(factor(ciudad)), 
    promocion_numerica = as.numeric(promocion)
  )

# Verificamos el resultado
glimpse(ventas_ecuador_transformado)

# VENTAS NORMALIZADAS ,UNIDADES NUMERICAS NORMALIZADAS

### Normalización de atributos numéricos
ventas_ecuador_transformado <- ventas_ecuador_transformado |>
  mutate(
    ventas_normalizadas = as.vector(scale(ventas)),
    unidades_vendidas_normalizadas = as.vector(scale(unidades_vendidas))
  )

# Verificamos el resultado
glimpse(ventas_ecuador_transformado)



#VENTAS POR MES (CUANTITATIVO)

# Resumen básico
summary(ventas_ecuador_enriquecido$fecha)
# Visualización de la distribución temporal
ggplot(ventas_ecuador_enriquecido, aes(x = fecha)) +
  geom_histogram(binwidth = 7, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Ventas a lo largo del tiempo",
       x = "Fecha", y = "Número de Ventas")

# Ventas por mes
ventas_ecuador_enriquecido |>
  mutate(mes = floor_date(fecha, "month")) |>
  group_by(mes) |>
  summarise(total_ventas = sum(ventas)) |>
  ggplot(aes(x = mes, y = total_ventas)) +
  geom_line() +
  geom_point() +
  labs(title = "Ventas Totales por Mes",
       x = "Mes", y = "Ventas totales")

#dia semana_finsemana

ventas_ecuador_transformado <- ventas_ecuador_transformado |>
  mutate(
    mes = month(fecha),
    dia_semana = wday(fecha),
    es_fin_semana = ifelse(dia_semana %in% c(1, 7), 1, 0),
    ventas_por_unidad = ventas / unidades_vendidas
  )
glimpse(ventas_ecuador_transformado)



#################


ggplot(ventas_ecuador_transformado, aes(x = es_fin_semana, y = ventas, group = es_fin_semana)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución de ventas los fines de semana",
       x = "Fin de semana (1 = sí, 0 = no)", y = "Ventas")


#___________________
#                       Numerización de atributos categóricos
#___________________

# Convertiremos algunas variables categóricas en numéricas:
ventas_ecuador_transformado <- ventas_ecuador_transformado |>
  mutate(
    ciudad_numerica = as.numeric(factor(ciudad)), 
    promocion_numerica = as.numeric(promocion)
  )