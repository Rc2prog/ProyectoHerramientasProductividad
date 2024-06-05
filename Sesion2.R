# Sesión 2 ----
## Ejercicios de 'reported_heights' usando tidyverse ----
library(dplyr)
library(dslabs)
data("reported_heights")

# Visualizar datos
reported_heights

# Guardar las alturar como números
reported_heights <- reported_heights |> 
  mutate(height2 = as.numeric(height))

# Acceder a datos usando corchetes
# por filas
reported_heights[1:5,]
# por columnas
reported_heights[,2]
# filas y columnas juntos
reported_heights[1:5,3]

## Filtrar usando dplyr ----
reported_heights |> 
  filter(height2 < 85)
reported_heights |> 
  filter(sex == "Female")

# Agregar una columna que convierta los valores de alturas en cm
reported_heights |>
  mutate(height_cm = height2 * 2.54) |> 
  summary()

## Juntar varios filtros con AND y OR ----
reported_heights |> 
  mutate(height_cm = height2 * 2.54) |> 
  filter(sex == "Male" | height_cm == 165.10)

reported_heights |> 
  mutate(height_cm = height2 * 2.54) |> 
  filter(height_cm < 210 & height_cm >= 130)

# Crear un dataset con datos coherentes
alturas_validas <- reported_heights |> 
  mutate(height_cm = height2 * 2.54) |> 
  filter(height_cm < 210 & height_cm >= 130 & !is.na(height_cm))
  


##  HASTA AQUÍ VA EL EJERCICIO DE LA CLASE SESIÓN 2 ###

# ¿Con cuántos datos nos estamos quedando?
summary(alturas_validas)
str(alturas_validas)

# Gráficos ----
## Gráfico de dispersión ----
plot(alturas_validas$height2, alturas_validas$height_cm)

## Histogramas ----
hist(reported_heights$height_cm)
hist(alturas_validas$height_cm)

## Boxplot ----
boxplot(reported_heights$height_cm ~ reported_heights$sex)
boxplot(alturas_validas$height_cm ~ alturas_validas$sex)

# Dataset 'coronavirus' ----
install.packages("coronavirus")
library(coronavirus)
data("coronavirus")

# Visualizar datos
coronavirus

# ¿Qué tipos de datos tenemos?
summary(coronavirus)

# ¿Hay NAs en el dataset?
sum(is.na(coronavirus))

# ¿Qué país tiene menos casos confirmados en un día? ¿Cuál tiene más casos?
coronavirus |> 
  select(country, type, cases) |> 
  filter(type == "confirmed") |> 
  arrange(desc(cases))

coronavirus |> 
  select(country, type, cases) |> 
  filter(type == "confirmed") |> 
  arrange(cases)

# Crear un dataframe con datos de México
mx <- coronavirus |> 
  filter(country == "Mexico")
mx

# Agrupar por años y calcular tasas de mortalidad
mx_año <- mx |> 
  mutate(date = as.Date(date),
         year = format(date, "%Y")) |> 
  filter(type == "death") |> 
  group_by(year) |> 
  summarise(defunciones = sum(cases),
            poblacion = mean(population)) |> 
  mutate(tasa = defunciones / poblacion * 100000)

# Fechas inicial y final del dataset
min(coronavirus$date)
max(coronavirus$date)

# en la fecha inicial se ven 0 casos confirmados, recuperados o difuntos
mx |> 
  filter(date == "2020-01-22")
# en la fecha final sí se ven casos confirmados, recuperados o difuntos
mx |> 
  filter(date == "2023-03-09")

## Gráficos sobre México ----
# Gráfico de dispersión
plot(mx$date, mx$cases)
plot(mx[mx$type == "confirmed", "date"], mx[mx$type == "confirmed","cases"])
lines(mx[mx$type == "confirmed", "date"], mx[mx$type == "confirmed","cases"])

mx_confirmed <- mx |> 
  filter(type == "confirmed")
plot(mx_confirmed$date, mx_confirmed$cases)
lines(mx_confirmed$date, mx_confirmed$cases)

# Gráfico de cajas
mx_box <- mx_confirmed |> 
  mutate(date = as.Date(date),
         year = format(date, "%Y"))
boxplot(mx_box$cases ~ mx_box$year)

# Gráfico de barras
summary(mx_box$year)
mx_year <- summary(as.factor(mx_box$year))

barplot(mx_year)

# Gráfico de pastel
pie(mx_year)
