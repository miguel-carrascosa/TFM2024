# Exploracion del dataset obtenido

library(tidyverse)
# library(readxl)
# install.packages("sjmisc")
library(sjmisc)
# barcos_bruto <- read_excel("./datos/Barcos_bruto.xlsx")
barcos_bruto <- read.csv(file="./datos/Barcos_bruto.csv",
                          sep=";")
barcos_bruto <- barcos_bruto |> 
  select(-c(Beam., Guests.))
# Mejor en csv
barcos <- barcos_bruto

barcos <- barcos |> select(-X) |> slice(-1) |> 
  rename_with(~tolower(gsub(".", " ", .x, fixed = TRUE)))

nombres_var_original <- dput(colnames(barcos))
nombres_var_fixed <- sub("\\s+$", "", nombres_var_original)
nombres_var_fixed <- gsub("  ", " ", nombres_var_fixed)
nombres_var_fixed <- gsub("\\s+", "_", nombres_var_fixed)
names(barcos) <- nombres_var_fixed

barcos_precios <- read.csv(file="./datos/Barcos_precios.csv",
                         sep=";")
barcos_precios <- barcos_precios |> select(-X) |> slice(-1) |> 
  rename_with(~tolower(gsub(".", " ", .x, fixed = TRUE))) |> 
  rename("name" = "boat name")

nombres_barcos <- barcos$name

barcos_precios <- barcos_precios |> 
  filter(name %in% nombres_barcos)

barcos <- merge(barcos, barcos_precios, by="name")

barcos <- barcos[!duplicated(barcos$name), ]


### Corregimos patrones de texto###

### Variables "length" y "beam" ###
barcos$length <- str_replace_all(barcos$length, " m", "")
barcos$beam <- str_replace_all(barcos$beam, " m", "")

### Variables "top_speed", "max_speed", "cruising_speed" y
### "x_1" ###
barcos$top_speed <- str_replace_all(barcos$top_speed, " kn", "")
barcos$max_speed <- str_replace_all(barcos$max_speed, " kn", "")
barcos$cruising_speed <- str_replace_all(barcos$cruising_speed,
                                         " kn", "")
barcos$x_1 <- str_replace_all(barcos$x_1,"(@ | kn)", "")

### Variables "length_overall", "length_at_waterline" y
### "max_draught" ###
barcos$length_overall <- 
  str_replace_all(barcos$length_overall, " metres", "")
barcos$length_at_waterline <- 
  str_replace_all(barcos$length_at_waterline, " metres", "")
barcos$max_draught <- 
  str_replace_all(barcos$max_draught, " metres", "")

### Variables "fuel_capacity" y "water_capacity" ###
barcos$fuel_capacity <- 
  str_replace_all(barcos$fuel_capacity, " litres", "")
barcos$water_capacity <- 
  str_replace_all(barcos$water_capacity, " litres", "")

### Variable "charter" ###
barcos$charter <-
  str_replace_all(barcos$charter, "(Price from | p/w •)", "")

### Variable "total_power" ###
barcos$total_power <-
  str_replace_all(barcos$total_power, "hp", "")

### Variable "range" ###
barcos$range <-
  str_replace_all(barcos$range, " nm", "")

### Variable "price" ###
barcos$price <-
  str_replace_all(barcos$price, " •", "")

### Variable "broker" ###
barcos$broker <-
  str_replace_all(barcos$broker, "(https://|http://|www.)", "")

barcos$broker <- sub("\\..*", "", barcos$broker)

# barcos |>
#   count(broker, sort = TRUE) |>
#   mutate(porc = 100*n/sum(n))

# Corregimos los últimos fallos de codificación del dataframe
barcos[barcos == ""] <- NA

# Convertimos algunas variables de texto a numéricas

barcos <-
  barcos |> 
  mutate(across(c(length, top_speed, beam, length_overall,
                  length_at_waterline, max_draught,
                  displacement_tonnage, max_speed,
                  cruising_speed, fuel_capacity, water_capacity,
                  total_power, range, x_1),
                as.double))

# Eliminamos las variables "year_of_build", "length_overall",
# "gross_tonnage", "max_speed" y "x_1"
barcos <-
  barcos |> select(-c(year_of_build, length_overall, gross_tonnage,
                      max_speed, x_1))

# Editamos la variable "price"
barcos$price[barcos$price == "POA"] <- NA
# Uno empieza por "A$"

# Seleccionamos las observaciones en las que "price" no tiene
# valores ausentes
barcos <-
  barcos |> filter(!is.na(price))

# Aplicaremos el tipo de cambio para pasar todos los precios a euros
# utilizando el promedio de enero de 2024
# fuente: https://www.exchange-rates.org/es/historial/eur-usd-2024
# fuente 2: https://www.exchange-rates.org/es/historial/eur-gbp-2024

# 1€ = 1.0909$
# 1€ = 0.8587£

barcos <- barcos |> 
  mutate(divisa=substr(price,1,1))

barcos$charter <- str_remove_all(barcos$charter,
                                  "(€|\\$|£|East-Med)")
barcos$price <- str_remove_all(barcos$price, "(€|\\$|£|A)")

barcos$charter <- as.numeric(gsub("[^0-9.]", "", barcos$charter))
barcos$price <- as.numeric(gsub("[^0-9.]", "", barcos$price))

barcos$charter <-
  ifelse(barcos$divisa %in% c("$", "A"), barcos$charter / 1.0909,
         ifelse(barcos$divisa == "£", barcos$charter / 0.8587,
                barcos$charter))

barcos$price <-
  ifelse(barcos$divisa %in% c("$", "A"), barcos$price / 1.0909,
         ifelse(barcos$divisa == "£", barcos$price / 0.8587,
                barcos$price))
# Finalmente eliminamos la columna "divisa"
barcos <-
  barcos |> select(-divisa)

#barcos |> select(where(is.numeric)) |> corrr::correlate() |> View()
# Punto de guardado

###############################################################

# Guardamos el progreso por si acaso
# install.packages("openxlsx")
library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/Barcos.xlsx"

# Write the dataframe to an Excel file
write.xlsx(barcos, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(barcos, "./datos/Barcos.csv")

# Lo volvemos a abrir
barcos2 <- read.csv(file="./datos/Barcos.csv",
                    sep=",")

# Todo en orden...

# barcos <- barcos |> select_if(!duplicated(colnames(barcos)))
###################################



