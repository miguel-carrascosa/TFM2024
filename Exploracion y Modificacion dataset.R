# Exploración y modificación "Barcos"

# Cargamos librerías
library(openxlsx)
library(broom)
library(outliers)
library(dummies)
# library(plyr)
library(tidyverse)
library(tidymodels)
library(sjmisc)
library(skimr)
# library(corrplot)
library(corrr)
library(plotly)
# library(scatterplot3d)
library(RColorBrewer)
library(moments)
# library(modeest)
# library(multimode)

barcos_bruto <- read.csv(file="./datos/Barcos.csv",
                    sep=",")

# barcos_bruto <- barcos_bruto |> mutate(across(where(is.logical), as.numeric))
# barcos |> summarise(across(where(is.numeric),\(barcos) mean(barcos,na.rm=TRUE)))
# kurtosis
# skewness
# barcos |> summarise(across(where(is.numeric),\(barcos) sum(!is.na(barcos))))
# barcos |> count(broker, sort = TRUE) |> mutate(porc = round(100*n/sum(n), digits = 2))

# barcos |>
#   count(broker, sort = TRUE) |>
#   mutate(porc = round(100*n/sum(n), digits = 2))

# barcos |> select(where(is.factor)) |> colnames() |> dput()
# c("broker", "yacht_type", "series_model_class", "builder", "naval_architect", 
#   "exterior_designer", "interior_designer", "hull_number", "hull_type", 
#   "classification", "engine_make", "hull", "superstructure", "deck", 
#   "refits", "yacht_subtype")


# Eliminamos "name", "single_rooms", "triple_rooms", convertible",
# "crew_bunk_beds", "pullman_beds", "mca_compliant", "engine_model"
# y "build_time"
barcos_bruto <-
  barcos_bruto |> select(-c(name, single_rooms, triple_rooms, convertible,
                      crew_bunk_beds, pullman_beds, mca_compliant,
                      engine_model, build_time))

barcos <- barcos_bruto

glimpse(barcos)
skim(barcos)

# Exploración valores únicos
unique_counts <- barcos %>%
  summarise_all(n_distinct) %>%
  gather(variable, unique_count)

# Create bar plot
fig <- plot_ly(unique_counts, x = ~unique_count, y = ~variable, type = 'bar') %>%
  layout(
    title = "Number of Unique Values per Variable",
    xaxis = list(title = "Number of Unique Values"),
    yaxis = list(title = "Variable"),
    plot_bgcolor = '#f2f2f2', # Background color
    hoverlabel = list(bgcolor = '#ffffff') # Tooltip background color
  )

fig

# RECATEGORIZAMOS LAS VARIABLES SEGÚN LO EXPUESTO EN EL EPÍGRAFE 5 DEL TFM
barcos <- barcos |> mutate(across(where(is.character), as_factor))

barcos <- barcos |> 
  # Variable broker
  mutate(broker =
           fct_lump_prop(broker, prop = 0.03,
                         other_level = "other_broker")) |> 
  # Variable yacht_type
  mutate(yacht_type = as.character(yacht_type)) |>  
  mutate(yacht_type =
           ifelse(is.na(yacht_type) | yacht_type != "Motor Yacht",
                  "Sail Yacht", "Motor Yacht")) |> 
  mutate(yacht_type = as_factor(yacht_type)) |>  
  # Variable series_model_class
  mutate(series_model_class =
           ifelse(is.na(series_model_class), 0, 1)) |> 
  # Variable naval_architect
  mutate(naval_architect =
           ifelse(is.na(naval_architect), 0, 1)) |> 
  # Variable exterior_designer
  mutate(exterior_designer =
           ifelse(is.na(exterior_designer), 0, 1)) |>   
  # Variable interior_designer
  mutate(interior_designer =
           ifelse(is.na(interior_designer), 0, 1)) |> 
  # Variable hull_number
  mutate(hull_number =
           ifelse(is.na(hull_number), 0, 1)) |>
  # Variable yacht_subtype
  mutate(yacht_subtype =
           ifelse(is.na(yacht_subtype), 0, 1)) |>
  # Variable hull_type
  mutate(hull_type =
           fct_lump_prop(hull_type, prop = 0.1,
                         other_level = "other_hull_type")) |> 
  mutate(hull_type = as.character(hull_type)) |> 
  mutate(hull_type =
           ifelse(is.na(hull_type), "unknown", hull_type)) |> 
  mutate(hull_type = as_factor(hull_type)) |> 
  # Variable classification
  mutate(classification =
           fct_lump_prop(classification, prop = 0.05,
                         other_level = "other_classification")) |> 
  mutate(classification = as.character(classification)) |> 
  mutate(classification =
           ifelse(is.na(classification), "unknown", classification)) |> 
  mutate(classification = as_factor(classification)) |>
  # Variable engine_make
  mutate(engine_make = as.character(engine_make)) |> 
  mutate(engine_make =
           case_when(
             grepl("(Caterpillar|CATERPILLAR|CAT|CAT C32 Acert|Caterpillar C32|Caterpillar C12)",
                   engine_make, ignore.case = TRUE) ~ "Caterpillar",
             grepl("(MTU|MTU 12V)", engine_make, ignore.case = TRUE) ~ "MTU",
             grepl("(MAN|MAN D2862)", engine_make, ignore.case = TRUE) ~ "MAN",
             is.na(engine_make) ~ "unknown",
             TRUE ~ "other_engine_make")) |>
  mutate(engine_make = as_factor(engine_make)) |> 
  # Variable hull
  mutate(hull = as.character(hull)) |> 
  mutate(hull =
           ifelse(hull %in% c("Aluminium","Aluminum"), "Aluminium",
                  ifelse(is.na(hull), "unknown", hull))) |> 
  mutate(hull = as_factor(hull)) |> 
  mutate(hull =
           fct_lump_prop(hull, prop = 0.1,
                         other_level = "other_hull")) |>
  # Variable superstructure
  mutate(superstructure = as.character(superstructure)) |> 
  mutate(superstructure =
           ifelse(superstructure %in% c("Aluminium","Aluminum"), "Aluminium",
                  ifelse(is.na(superstructure), "unknown", superstructure))) |> 
  mutate(superstructure = as_factor(superstructure)) |> 
  mutate(superstructure =
           fct_lump_prop(superstructure, prop = 0.1,
                         other_level = "other_superstructure")) |> 
  # Variable deck
  mutate(deck = as.character(deck)) |> 
  mutate(deck =
           ifelse(is.na(deck), "unknown", deck)) |> 
  mutate(deck = as_factor(deck)) |> 
  mutate(deck =
           fct_lump_prop(deck, prop = 0.1,
                         other_level = "other_deck"))

# En el caso de la variable refits crearemos 2 nuevas variables: una binaria
# y otra numérica entera que cuente el número de reacondicionamientos
# suponiendo que los datos ausentes valgan 0

barcos <- barcos |> 
  mutate(refits_bin =
           ifelse(is.na(refits), 0, 1)) |> 
  mutate(refits_num =
           ifelse(is.na(refits), 0,
                  str_count(refits, pattern = "-")/2)) |> 
  select(-refits)

# En la variable builder crearemos 4 categorías en base a los precios
# medios de cada fabricante y su posición en la distribución por cuartiles
# de la variable price

tabla <- barcos |>
  group_by(builder) |>
  summarise(precio_medio = mean(price)) |>
  mutate(category =
           case_when(precio_medio <= 2979191.5 ~ "low_cost",
                     precio_medio <= 6500000 ~ "standard",
                     precio_medio <= 13750057.3 ~ "upper_standard",
                     precio_medio > 13750057.3 ~ "supreme")) |> 
  ungroup()

lookup_table <- setNames(tabla$category, tabla$builder)
barcos$builder <- lookup_table[barcos$builder]

# barcos |> group_by(builder) |> summarise(precio_medio = mean(price)) 

barcos <- barcos |> 
  mutate(builder = as.character(builder)) |> 
  mutate(builder = ifelse(is.na(builder), "upper_standard", builder)) |> 
  mutate(builder = as_factor(builder))


# Exploración variables numéricas discretas
barcos |>
  select(build_year, guests, crew, passenger_rooms, master_rooms,
         double_rooms, twin_rooms, number_of_decks, vip_rooms,
         refits_num) |> 
  skim()

var_disc <- c("build_year", "guests", "crew", "passenger_rooms", "master_rooms",
              "double_rooms", "twin_rooms", "number_of_decks", "vip_rooms",
              "refits_num")

for(i in var_disc){
  barcos <-
    barcos |> 
    mutate(!!i :=
             ifelse(is.na(!!sym(i)),
                    median(!!sym(i), na.rm=TRUE),
                    !!sym(i)))
}
# 19 guests, 28 crew, 15 passenger_rooms

# Gráficos de barras de las variables discretas
library(RColorBrewer)
nb.cols <- 19
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)

ggplot(barcos, aes(x = as_factor(guests),
                        fill = as_factor(guests))) +
  geom_bar(color = "black", alpha = 1) +
  scale_fill_manual(values = mycolors) +
  labs(title = "Diagrama de barras (guests)",
       x = "Valores de la variable guests", y = "Frecuencia") +
  theme_gray()

# Subplots de gráficos de barras de variables discretas
library(plotly)
# Función para crear tablas de frecuencias por variable
create_freq_table <- function(data, variable) {
  table(data[[variable]])
}
# Creamos una lista para guardar los dataframes agrupados
grouped_data <- lapply(c("build_year", "guests", "crew", "passenger_rooms", 
                         "master_rooms", "double_rooms", "twin_rooms", 
                         "number_of_decks", "vip_rooms", "refits_num"),
                       function(var) {
                           # Datos agrupados por variable actual
                           grouped <- barcos |> 
                             group_by({{ var }})
                           
                           # Devolvemos los dataframes agrupados
                           return(grouped)
                         })
# Creamos gráficos separados para cada variable
plots <- lapply(seq_along(grouped_data), function(i) {
  var <- c("build_year", "guests", "crew", "passenger_rooms", 
           "master_rooms", "double_rooms", "twin_rooms", 
           "number_of_decks", "vip_rooms", "refits_num")[i]
  # Calculamos frecuencias para cada valor único
  freq_table <- create_freq_table(grouped_data[[i]], var)
  freq_df <- as.data.frame(freq_table)
  names(freq_df) <- c(var, "Frequency")
  freq_df <- freq_df |> 
    arrange(desc(Frequency))
  # Creamos el gráfico de barras
  plot <- plot_ly(freq_df, x = ~.data[[var]], y = ~Frequency, type = 'bar') |> 
    layout(
      xaxis = list(title = var),
      yaxis = list(title = "Frequency"),
      plot_bgcolor = '#f2f2f2', # Color de fondo (background)
      hoverlabel = list(bgcolor = '#ffffff'), # Tooltip color de fondo
      showlegend = FALSE  # Eliminamos la leyenda
    )
  
  # Devolvemos el gráfico
  return(plot)
})
# Creamos subplots con los gráficos del bucle anterior
fig <- subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
               plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]],
               nrows = 5) |> 
  layout(title = "Frecuencia de Valores Únicos")

# Le damos un título a cada subplot a través de anotaciones
annotations = list(
  list(x = 0.2, y = 1.0, text = paste("<b>", var_disc[1], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.8, y = 1.0, text = paste("<b>", var_disc[2], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.2, y = 0.7, text = paste("<b>", var_disc[3], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.8, y = 0.7, text = paste("<b>", var_disc[4], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.2, y = 0.5, text = paste("<b>", var_disc[5], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.8, y = 0.5, text = paste("<b>", var_disc[6], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.2, y = 0.3, text = paste("<b>", var_disc[7], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.8, y = 0.3, text = paste("<b>", var_disc[8], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.2, y = 0.1, text = paste("<b>", var_disc[9], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE),
  list(x = 0.8, y = 0.1, text = paste("<b>", var_disc[10], "</b>"),
       xref = "paper", yref = "paper", xanchor = "center",
       yanchor = "bottom", showarrow = FALSE))

# Unimos todo y lo representamos
fig <- fig |> layout(annotations = annotations)
fig

#### Subplots de scatter plots de variables discretas ####

plots <- lapply(seq_along(grouped_data), function(i) {
  var <- c("build_year", "guests", "crew", "passenger_rooms", 
           "master_rooms", "double_rooms", "twin_rooms", 
           "number_of_decks", "vip_rooms", "refits_num")[i]
  
  # Comprobamos que la variable existe en el dataframe
  if (var %in% names(barcos)) {
    # Creamos el scatter plot
    plot <- plot_ly(barcos, x = ~barcos[[var]], y = ~price, type = 'scatter', alpha = 0.65, mode = "markers")
    
    # Usamos la función fit() del paquete tidymodels
    lm_formula <- reformulate(var, "price")
    lm_model <- lm(lm_formula, data = barcos)
    
    x_range <- seq(min(barcos[[var]]), max(barcos[[var]]))
    x_range <- matrix(x_range)
    xdf <- data.frame(x_range)
    colnames(xdf) <- var
    ydf <- predict(lm_model, newdata = xdf)
    xy <- data.frame(xdf, ydf)
    
    # Incluimos las ecuaciones y los R-cuadrados
    equation <- paste("y = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), " * x")
    r_squared <- paste("R^2 = ", round(summary(lm_model)$r.squared, 3))
    annotation <- list(
      x = ifelse(i <= 5, 0.3, 0.7), 
      y = ifelse(i <= 6, 0.55, 0.4), 
      text = paste(equation, "<br>", r_squared), 
      xref = "paper", 
      yref = "paper", 
      showarrow = FALSE
    )
    
    plot <- plot %>% add_trace(data = xy, x = xdf[, 1], y = ydf, name = 'Regression Fit', mode = 'lines', alpha = 1) %>%
      layout(
        xaxis = list(title = var),
        yaxis = list(title = "Price"),
        showlegend = FALSE,  # Eliminamos la leyenda
        plot_bgcolor = '#f2f2f2', # Color de fondo (background)
        hoverlabel = list(bgcolor = '#ffffff'), # Tooltip color de fondo
        xref = "paper",
        yref = "paper",
        annotations = list(annotation)
      )
    
    # Devolvemos el gráfico
    return(plot)
  } else {
    print(paste("Variable", var, "no encontrada en el dataset."))
  }
}) 

# Combinamos los gráficos en un mismo subplot
fig <- subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
               plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]],
               nrows = 5) |> 
  layout(title = "Scatter plot de Price sobre variables discretas")

annotations = list(
  list(x = 0.2, y = 1.0, text = paste("<b>", var_disc[1], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 1.0, text = paste("<b>", var_disc[2], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.75, text = paste("<b>", var_disc[3], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.75, text = paste("<b>", var_disc[4], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.55, text = paste("<b>", var_disc[5], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.55, text = paste("<b>", var_disc[6], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.35, text = paste("<b>", var_disc[7], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.35, text = paste("<b>", var_disc[8], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.15, text = paste("<b>", var_disc[9], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.15, text = paste("<b>", var_disc[10], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE))

fig <- fig |> layout(annotations = annotations)
fig




#### Exploración variables numéricas continuas ####
var_cont <- c("length", "top_speed", "beam", "gt", "charter",
              "length_at_waterline", "max_draught", "displacement_tonnage",
              "cruising_speed", "fuel_capacity", "water_capacity",
              "total_power", "range")

barcos |>
  select(all_of(var_cont)) |> 
  skim()

cor(barcos$length, barcos$length_at_waterline, use="complete.obs")

# Eliminamos length_at_waterline y transformamos charter
barcos <- barcos |> 
  select(-length_at_waterline)

barcos <- barcos |> 
  mutate(charter = ifelse(is.na(charter),0,1))

# Imputamos la mediana para el resto de variables continuas
var_cont <- c("length", "top_speed", "beam", "gt","max_draught",
              "displacement_tonnage", "cruising_speed",
              "fuel_capacity", "water_capacity", "total_power",
              "range")

for(i in var_cont){
  barcos <-
    barcos |> 
    mutate(!!i :=
             ifelse(is.na(!!sym(i)),
                    median(!!sym(i), na.rm=TRUE),
                    !!sym(i)))
}

barcos |>
  select(all_of(var_cont)) |> 
  skim()

var_cont_1 <- var_cont[1:6]
var_cont_2 <- var_cont[7:length(var_cont)]

# Creamos histogramas y funciones de densidad
# Definimos colores para las funciones de densidad

hist_colors <- c("#fa8072", "#87cefa", "#90ee90", "#ffae42",
                 "#dda0dd", "#cd853f")

# Creamos subplots para cada variable
plots <- lapply(seq_along(var_cont_1), function(i) {
  var <- var_cont_1[i]
  
  # Creamos histogramas con ggplot2
  hist_plot <- ggplot(barcos, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), fill = hist_colors[i]) +
    geom_density(alpha = 0.5, color = "black") +
    labs(x = var, y = "Density") +
    theme_gray()
  
  # Convertimos el gráfico de ggplot2 en un plotly object
  p <- ggplotly(hist_plot)
  
  # Devolvemos el gráfico
  return(p)
})

# Creamos subplots
fig <- subplot(plots, nrows = 3) %>%
  layout(title = "Histogramas y funciones de densidad")

annotations = list(
  list(x = 0.2, y = 1.0, text = paste("<b>", var_cont[1], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 1.0, text = paste("<b>", var_cont[2], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.6, text = paste("<b>", var_cont[3], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.6, text = paste("<b>", var_cont[4], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.25, text = paste("<b>", var_cont[5], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.25, text = paste("<b>", var_cont[6], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE))

fig <- fig |> layout(annotations = annotations)
fig


# Repetimos el proceso con el resto de variables continuas

hist_colors <- c("#ffc40c", "#c32148", "#40e0d0", "#bf00ff",
                 "#bfc1c2")

# Creamos subplots para cada variable
plots <- lapply(seq_along(var_cont_2), function(i) {
  var <- var_cont_2[i]
  
  hist_plot <- ggplot(barcos, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), fill = hist_colors[i]) +
    geom_density(alpha = 0.5, color = "black") +
    labs(x = var, y = "Density") +
    theme_gray()
  
  p <- ggplotly(hist_plot)
  return(p)
})

# Creamos subplots
fig <- subplot(plots, nrows = 3) %>%
  layout(title = "Histogramas y funciones de densidad")

annotations = list(
  list(x = 0.2, y = 1.0, text = paste("<b>", var_cont[7], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 1.0, text = paste("<b>", var_cont[8], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.6, text = paste("<b>", var_cont[9], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.6, text = paste("<b>", var_cont[10], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.25, text = paste("<b>", var_cont[11], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE))

fig <- fig |> layout(annotations = annotations)
fig




#### Subplots de scatter plots de variables continuas ####

plots <- lapply(seq_along(var_cont_1), function(i) {
  var <- var_cont_1[i]
  
  # Comprobamos que la variable existe en el dataframe
  if (var %in% names(barcos)) {
    # Creamos el scatter plot
    plot <- plot_ly(barcos, x = ~barcos[[var]], y = ~price, type = 'scatter', alpha = 0.65, mode = "markers")
    
    # Usamos la función fit() del paquete tidymodels
    lm_formula <- reformulate(var, "price")
    lm_model <- lm(lm_formula, data = barcos)
    
    x_range <- seq(min(barcos[[var]]), max(barcos[[var]]))
    x_range <- matrix(x_range)
    xdf <- data.frame(x_range)
    colnames(xdf) <- var
    ydf <- predict(lm_model, newdata = xdf)
    xy <- data.frame(xdf, ydf)
    
    # Incluimos las ecuaciones y los R-cuadrados
    equation <- paste("y = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), " * x")
    r_squared <- paste("R^2 = ", round(summary(lm_model)$r.squared, 3))
    annotation <- list(
      x = ifelse(i <= 5, 0.3, 0.7), 
      y = ifelse(i <= 6, 0.55, 0.4), 
      text = paste(equation, "<br>", r_squared), 
      xref = "paper", 
      yref = "paper", 
      showarrow = FALSE
    )
    
    plot <- plot %>% add_trace(data = xy, x = xdf[, 1], y = ydf, name = 'Regression Fit', mode = 'lines', alpha = 1) %>%
      layout(
        xaxis = list(title = var),
        yaxis = list(title = "Price"),
        showlegend = FALSE,  # Eliminamos la leyenda
        plot_bgcolor = '#f2f2f2', # Color de fondo (background)
        hoverlabel = list(bgcolor = '#ffffff'), # Tooltip color de fondo
        xref = "paper",
        yref = "paper",
        annotations = list(annotation)
      )
    
    # Devolvemos el gráfico
    return(plot)
  } else {
    print(paste("Variable", var, "no encontrada en el dataset."))
  }
}) 

# Combinamos los gráficos en un mismo subplot
fig <- subplot(plots, nrows = 3) |> 
  layout(title = "Scatter plot de Price sobre variables continuas")

annotations = list(
  list(x = 0.2, y = 1.0, text = paste("<b>", var_cont[1], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 1.0, text = paste("<b>", var_cont[2], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.6, text = paste("<b>", var_cont[3], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.6, text = paste("<b>", var_cont[4], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.25, text = paste("<b>", var_cont[5], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.25, text = paste("<b>", var_cont[6], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE))

fig <- fig |> layout(annotations = annotations)
fig


# Repetimos el proceso con el resto de variables continuas
plots <- lapply(seq_along(var_cont_2), function(i) {
  var <- var_cont_2[i]
  
  # Comprobamos que la variable existe en el dataframe
  if (var %in% names(barcos)) {
    # Creamos el scatter plot
    plot <- plot_ly(barcos, x = ~barcos[[var]], y = ~price, type = 'scatter', alpha = 0.65, mode = "markers")
    
    # Usamos la función fit() del paquete tidymodels
    lm_formula <- reformulate(var, "price")
    lm_model <- lm(lm_formula, data = barcos)
    
    x_range <- seq(min(barcos[[var]]), max(barcos[[var]]))
    x_range <- matrix(x_range)
    xdf <- data.frame(x_range)
    colnames(xdf) <- var
    ydf <- predict(lm_model, newdata = xdf)
    xy <- data.frame(xdf, ydf)
    
    # Incluimos las ecuaciones y los R-cuadrados
    equation <- paste("y = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), " * x")
    r_squared <- paste("R^2 = ", round(summary(lm_model)$r.squared, 3))
    annotation <- list(
      x = ifelse(i <= 5, 0.3, 0.7), 
      y = ifelse(i <= 6, 0.55, 0.4), 
      text = paste(equation, "<br>", r_squared), 
      xref = "paper", 
      yref = "paper", 
      showarrow = FALSE
    )
    
    plot <- plot %>% add_trace(data = xy, x = xdf[, 1], y = ydf, name = 'Regression Fit', mode = 'lines', alpha = 1) %>%
      layout(
        xaxis = list(title = var),
        yaxis = list(title = "Price"),
        showlegend = FALSE,  # Eliminamos la leyenda
        plot_bgcolor = '#f2f2f2', # Color de fondo (background)
        hoverlabel = list(bgcolor = '#ffffff'), # Tooltip color de fondo
        xref = "paper",
        yref = "paper",
        annotations = list(annotation)
      )
    
    # Devolvemos el gráfico
    return(plot)
  } else {
    print(paste("Variable", var, "no encontrada en el dataset."))
  }
}) 

# Combinamos los gráficos en un mismo subplot
fig <- subplot(plots, nrows = 3) |> 
  layout(title = "Scatter plot de Price sobre variables continuas")

annotations = list(
  list(x = 0.2, y = 1.0, text = paste("<b>", var_cont[7], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 1.0, text = paste("<b>", var_cont[8], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.6, text = paste("<b>", var_cont[9], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.8, y = 0.6, text = paste("<b>", var_cont[10], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE),
  list(x = 0.2, y = 0.25, text = paste("<b>", var_cont[11], "</b>"), xref = "paper",
       yref = "paper", xanchor = "center", yanchor = "bottom",
       showarrow = FALSE))

fig <- fig |> layout(annotations = annotations)
fig



# Regresiones lineales simples antes y después de depuración de
# outliers

library(broom)
reg_lineal <-
  lm(price ~ number_of_decks,
     data = barcos2)

summary(reg_lineal)
glance(reg_lineal) |> View()


# Detectaremos outliers con respecto a la mediana
library(outliers)
var_outliers <- c("build_year", "guests", "crew", "passenger_rooms",
                  "length", "top_speed", "beam",
                  "max_draught", "cruising_speed", "fuel_capacity",
                  "water_capacity", "total_power", "range")
barcos2 <- barcos
# barcos2 |> select(all_of(var_outliers)) |> skim()

# Detección de outliers con respecto a la mediana y conversión a NA
for (i in var_outliers) {
  # Calculamos mediana y MAD (Median Absolute Deviation)
  med <- median(barcos2[[i]], na.rm = TRUE)
  mad <- mad(barcos2[[i]], constant = 1.4826, na.rm = TRUE)
  
  # Detectamos outliers y convertimos a NA
  barcos2[[i]] <- ifelse(abs(barcos2[[i]] - med) > 3 * mad, NA, barcos2[[i]])
}

# Imputamos la mediana en los valores ausentes
for (i in var_outliers) {
  # Calculamos la mediana excluyendo los NAs
  med <- median(barcos2[[i]], na.rm = TRUE)
  
  # Imputamos la mediana donde haya NAs
  barcos2[[i]] <- ifelse(is.na(barcos2[[i]]), med, barcos2[[i]])
}

# Imputación de la mediana en el único valor extremo de
# number_of_decks
barcos2$number_of_decks[barcos2$number_of_decks ==
                          max(barcos2$number_of_decks)] <-
  median(barcos2$number_of_decks)



# Estudiaremos la distribución de price en función del valor de
# las variables binarias

var_bin <- c("series_model_class", "naval_architect",
             "exterior_designer", "interior_designer", "hull_number",
             "yacht_subtype", "refits_bin", "charter")

price_dist_bin <- data.frame()

for (i in var_bin) {
  nueva_fila <-
    barcos |>
    group_by(barcos[[i]]) |>
    skim(price)
  
  price_dist_bin <- rbind(price_dist_bin, nueva_fila)
}

#### Tratamiento final de outliers ####

# Filtro de Hampel para variables "guests", "passenger_rooms" y "max_draught"
var_outliers <- c("guests", "passenger_rooms", "max_draught")
# Detección de outliers
for (i in var_outliers) {
  med <- median(barcos[[i]], na.rm = TRUE)
  mad <- mad(barcos[[i]], constant = 1.4826, na.rm = TRUE)
  barcos[[i]] <- ifelse(abs(barcos[[i]] - med) > 3 * mad, NA, barcos[[i]])
}
# Imputación de la mediana
for (i in var_outliers) {
  med <- median(barcos[[i]], na.rm = TRUE)
  barcos[[i]] <- ifelse(is.na(barcos[[i]]), med, barcos[[i]])
}

# Sustitución del máximo por la mediana en las variables "length",
# "beam", "fuel_capacity" y "water_capacity"
var_outliers <- c("length", "beam", "fuel_capacity", "water_capacity")

# barcos$length[barcos$length ==
#                 max(barcos$length)] <-
#   median(barcos$length)

for (i in var_outliers) {
  barcos[[i]][barcos[[i]] == max(barcos[[i]])] <- median(barcos[[i]])
}

#### Normalización de variables discretas y continuas ####
var_cuali <- barcos |> select(where(is.factor)) |> colnames()
var_num <- barcos |> 
  select(-price) |> 
  select_if(function(x) is.numeric(x) && !all(x %in% c(0, 1))) |> 
  colnames()
var_bin <- barcos |> 
  select_if(function(x) is.numeric(x) && all(x %in% c(0, 1))) |> 
  colnames()
vardep <- "price"

# Función de normalización Min-Max
min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

barcos2 <- as.data.frame(lapply(barcos[,var_num], min_max))
barcos<-data.frame(cbind(barcos2,barcos[,c(var_bin,var_cuali,vardep)]))

#### Dummyficación de variables cualitativas ####
library(dummies)

# Valores únicos (45 en 9 variables)
var_cuali <- barcos |> select(where(is.factor)) |> colnames()
for(i in var_cuali){print(barcos[[i]] |> n_distinct())}

barcos2 <- dummy.data.frame(barcos, var_cuali, sep = ".")

# Pasamos de tener 39 columnas a 75

#### Matriz de correlaciones ####
library(corrr)
cor_matrix <-
  correlate(barcos2, diagonal = 1)

high_corr <- cor_matrix |> 
  stretch() |> 
  arrange(r)

high_corr <- high_corr |> 
  filter(r<1 & abs(r)>0.9)

# Eliminamos "yacht_type.Sail Yacht", "hull.unknown", "deck.unknown",
# "cruising_speed" y "hull.GRP"
barcos2 <- barcos2 |> 
  select(-c("yacht_type.Sail Yacht", "hull.unknown",
            "deck.unknown", "cruising_speed", "hull.GRP"))

# Lista de Frecuencias de las categóricas, útil pues algunos niveles
# con pocas observaciones no deben ser tenidos en cuenta
# en modelos de machine learning para evitar sobreajuste
# library(plyr)

frecu<-plyr::ldply(barcos[,var_cuali],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)

frecu |> arrange(frecuencia)

# Los valores menos frecuentes son worthavenueyachts y iyc de la variable
# "broker", y other_hull_type de la variable "hull_type", todos ellos
# con 30 observaciones cada uno, lo que es igual al 4.32% del total de las
# 695 observaciones

#### Guardamos los datos hasta ahora ####
library(openxlsx)

# Specify the file path where you want to save the Excel file
file_path <- "./datos/Barcos2.xlsx"

# Write the dataframe to an Excel file
write.xlsx(barcos2, file_path, sheetName = "Sheet1", rowNames = FALSE)
write_csv(barcos2, "./datos/Barcos2.csv")



#### Volvemos a abrir el archivo para cargar los datos desde este punto ####
barcos <- read.csv(file="./datos/Barcos2.csv",
                   sep=",")


#### Selección de variables ####
library(caret)
library(parallel)
library(doParallel)
library(MASS)
source("./funciones/funcion steprepetido.R")
library(Boruta)
library(MXM)
nombres1 <- barcos |> 
  select(-price) |> 
  colnames()
vardep <- "price"

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing

### Filtro SBF (paquete caret)

# Preparo matrices X e Y

archivo1<-barcos
y<-archivo1[,vardep]
x<-archivo1[,nombres1]

# Aplico el filtro

filtro<-sbf(x,y,sbfControl = sbfControl(functions = rfSBF,
                                        method = "cv",
                                        verbose = FALSE))
a<-dput(filtro$optVariables)
length(a)

# Selecciona 45 variables

# c("length", "build_year", "top_speed", "beam", "gt", "guests", 
#   "crew", "max_draught", "displacement_tonnage", "fuel_capacity", 
#   "water_capacity", "total_power", "passenger_rooms", "double_rooms", 
#   "twin_rooms", "number_of_decks", "range", "vip_rooms", "refits_num", 
#   "charter", "series_model_class", "naval_architect", "hull_number", 
#   "broker.denisonyachtsales", "broker.burgessyachts", "yacht_type.Motor.Yacht", 
#   "builder.low_cost", "builder.upper_standard", "builder.standard", 
#   "builder.supreme", "hull_type.unknown", "hull_type.Deep.V...Planing", 
#   "hull_type.Full.Displacement", "hull_type.Semi.Displacement", 
#   "hull_type.Monohull", "classification.unknown", "classification.LR", 
#   "engine_make.Caterpillar", "engine_make.other_engine_make", "hull.Steel", 
#   "hull.other_hull", "superstructure.GRP", "superstructure.Aluminium", 
#   "deck.Teak", "deck.other_deck")

### StepAIC y StepBIC

# Con AIC

full<-glm(price~.,data=archivo1,family = gaussian(link = "identity"))
null<-glm(price~1,data=archivo1,family = gaussian(link = "identity"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = gaussian(link = "identity"),
                trace=FALSE)

vec<-(names(selec1[[1]]))

length(vec)

dput(vec)

# Selecciona 26-1 variables

# c("(Intercept)", "gt", "build_year", "length", "water_capacity", 
#   "displacement_tonnage", "engine_make.unknown", "total_power", 
#   "hull_number", "engine_make.Caterpillar", "vip_rooms", "builder.supreme", 
#   "crew", "refits_num", "classification.unknown", "classification.LR", 
#   "refits_bin", "beam", "deck.other_deck", "broker.camperandnicholsons", 
#   "hull_type.Semi.Displacement", "yacht_type.Motor.Yacht", "number_of_decks", 
#   "guests", "twin_rooms", "fuel_capacity")


# Con BIC
# Ponemos k=log(n) en stepAIC,
# en este caso n=695 observaciones

full<-glm(price~.,data=archivo1,family = gaussian(link = "identity"))
null<-glm(price~1,data=archivo1,family = gaussian(link = "identity"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = gaussian(link = "identity"),
                trace=FALSE, k=6)

vec<-(names(selec1[[1]]))

length(vec)

dput(vec)

# Selecciona 12-1 variables

# c("(Intercept)", "gt", "build_year", "length", "water_capacity", 
#   "displacement_tonnage", "engine_make.unknown", "total_power", 
#   "hull_number", "engine_make.Caterpillar", "vip_rooms", "builder.supreme")




### Stepwise repetido

# La función steprepetido permite realizar el proceso training test
# varias veces obteniendo el modelo por stepwise sobre datos train
# y la tabla de frecuencias de los modelos escogidos

lista<-steprepetido(data=archivo1,vardep=vardep,
                    listconti=nombres1,
                    sinicio=12345,sfinal=12385,
                    porcen=0.8,criterio="BIC")

tabla<-lista[[1]]
dput(lista[[2]][[1]])

# c("gt", "build_year", "length", "water_capacity", "displacement_tonnage", 
#   "engine_make.unknown", "hull_number", "total_power", "engine_make.Caterpillar", 
#   "vip_rooms")

dput(lista[[2]][[2]])

# c("gt", "build_year", "length", "water_capacity", "engine_make.unknown", 
#   "displacement_tonnage", "total_power", "hull_number", "engine_make.Caterpillar")



### Wrapper RFE

control <- rfeControl(functions=rfFuncs, method="cv", number=10, repeats=5)
results <- rfe(x, y, sizes=c(1:8), rfeControl=control)

selecrfe<-results$optVariables
length(selecrfe)
dput(selecrfe)

# Escoge 4 variables

# c("build_year", "gt", "length", "beam")


### Boruta (usa random forest y también vale como filter)

out.boruta <- Boruta(price~., data = archivo1)

print(out.boruta)

summary(out.boruta)

sal<-data.frame(out.boruta$finalDecision)

sal2 <- sal |> filter(out.boruta.finalDecision == "Confirmed")
dput(row.names(sal2))

length(dput(row.names(sal2)))

# 23 variables

# c("length", "build_year", "top_speed", "beam", "gt", "guests", 
#   "crew", "max_draught", "displacement_tonnage", "fuel_capacity", 
#   "water_capacity", "total_power", "passenger_rooms", "number_of_decks", 
#   "range", "builder.low_cost", "builder.upper_standard", "builder.standard", 
#   "builder.supreme", "hull_type.unknown", "hull_type.Full.Displacement", 
#   "hull.Steel", "superstructure.unknown")



### MXM

# MMPC

mmpc1 <- MMPC(vardep, archivo1, max_k = 3, hash = TRUE,
              test = "testIndFisher")

mmpc1@selectedVars

a<-dput(names(archivo1[,c(mmpc1@selectedVars)]))

length(a)

# 5 variables

# c("length", "build_year", "gt", "displacement_tonnage", "water_capacity")


# SES

SES1 <- SES(vardep, archivo1, max_k = 3, hash = TRUE,
            test = "testIndFisher")

SES1@selectedVars

dput(names(archivo1[,c(SES1@selectedVars)]))

a<-dput(names(archivo1[,c(SES1@selectedVars)]))

length(a)

# 4 variables

# c("length", "build_year", "gt", "displacement_tonnage")

# Alternativamente, podemos cambiar el p valor de corte con threshold:

SES1 <- SES(vardep, archivo1, max_k = 8, hash = TRUE,threshold=0.01,
            test = "testIndFisher")

SES1@selectedVars

dput(names(archivo1[,c(SES1@selectedVars)]))

a<-dput(names(archivo1[,c(SES1@selectedVars)]))

length(a)

# Aun así nos da lo mismo
# c("length", "build_year", "gt", "displacement_tonnage")



### PRUEBA CON BAGGING

rfgrid<-expand.grid(mtry=c(27))

control<-trainControl(method = "repeatedcv",number=4,repeats=5,
                      savePredictions = "all") 

rf<- caret::train(price~.,data=archivo1,
                  method="rf",trControl=control,tuneGrid=rfgrid,
                  linOut = TRUE,ntree=300,nodesize=10,replace=TRUE,
                  importance=TRUE)
rf

### CON IMPORTANCIA DE VARIABLES RANDOM FOREST
library(randomForest)
final<-rf$finalModel
tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$IncNodePurity),]
tabla

#                                          %IncMSE IncNodePurity
# gt                                  14.844335867  6.854198e+16
# length                              10.739487686  6.461539e+16
# beam                                11.923536210  3.363911e+16
# crew                                 5.406487455  1.298816e+16
# build_year                          19.380922705  6.667102e+15
# passenger_rooms                      1.430994250  5.727359e+15
# number_of_decks                      1.384838299  4.345449e+15
# fuel_capacity                        4.076019599  3.576772e+15
# water_capacity                       3.777934014  3.124600e+15
# refits_num                           0.280557821  2.273512e+15
# guests                               0.125390742  2.206295e+15
# builder.supreme                      4.504324401  2.018543e+15
# top_speed                            2.753395755  1.696034e+15
# max_draught                          3.901661248  1.544442e+15
# displacement_tonnage                 5.249691777  1.404503e+15
# engine_make.unknown                  3.055096236  1.264039e+15
# total_power                          2.462954848  1.031027e+15
# range                                3.299921836  9.434264e+14
# vip_rooms                            5.024968716  6.960727e+14
# interior_designer                    2.304854770  5.837810e+14
# hull_type.other_hull_type            2.664004894  5.722697e+14
# hull.Steel                           2.140366309  4.689223e+14
# builder.upper_standard               3.624888053  4.148362e+14
# hull_type.Full.Displacement          0.594027055  3.925965e+14
# broker.burgessyachts                 0.677339020  3.765717e+14
# engine_make.Caterpillar              1.439938316  3.416642e+14
# charter                              0.888692361  3.311417e+14
# engine_make.other_engine_make        1.782479097  3.143787e+14
# double_rooms                         2.340610574  3.036447e+14
# builder.low_cost                     6.531716100  2.769100e+14
# refits_bin                           2.199287208  2.623535e+14
# superstructure.unknown               5.044215511  2.421049e+14
# yacht_subtype                        0.181404960  2.355719e+14
# twin_rooms                           3.079456177  2.213573e+14
# exterior_designer                    2.229842485  2.180946e+14
# classification.RINA                 -0.704158270  2.121592e+14
# engine_make.MTU                      0.732371176  1.823817e+14
# hull_type.unknown                    2.558742098  1.719660e+14
# hull_number                          3.807025910  1.630346e+14
# naval_architect                      2.249654862  1.437933e+14
# classification.unknown               1.570755238  1.379588e+14
# classification.LR                    1.702753972  1.283468e+14
# broker.camperandnicholsons           0.961110993  1.145928e+14
# broker.northropandjohnson           -0.383274517  1.036737e+14
# deck.other_deck                      3.254496502  1.026065e+14
# series_model_class                   2.983985474  1.000354e+14
# superstructure.other_superstructure  2.249035836  9.734259e+13
# superstructure.Aluminium             1.008862948  8.926793e+13
# builder.standard                     2.445433480  8.727340e+13
# hull_type.Semi.Displacement          1.668451554  7.981856e+13
# classification.other_classification -1.600005587  7.365540e+13
# broker.other_broker                  0.178439062  6.947552e+13
# deck.Teak                            1.260137243  6.144518e+13
# hull.Aluminium                       1.549117137  5.960035e+13
# yacht_type.Motor.Yacht               2.374481206  5.851832e+13
# hull_type.Monohull                   2.161096933  5.246224e+13
# classification.BV                    1.224018609  5.240041e+13
# broker.edmiston                     -1.097292398  4.560166e+13
# classification.ABS                   0.002378019  4.295069e+13
# hull_type.Deep.V...Planing           1.728410502  4.211643e+13
# engine_make.MAN                      2.367123446  3.866877e+13
# superstructure.GRP                   2.602682892  3.824949e+13
# broker.fraseryachts                  0.335238992  3.500968e+13
# broker.worthavenueyachts             0.919363738  3.352595e+13
# broker.iyc                          -2.137685520  2.904811e+13
# hull.other_hull                      1.857254903  2.555445e+13
# broker.oceanindependence            -0.503472220  1.234344e+13
# broker.denisonyachtsales            -0.874947332  8.322367e+12
# master_rooms                        -1.594808465  3.046968e+12

lista<-dput(rownames(tabla))

# c("gt", "length", "beam", "crew", "build_year", "passenger_rooms", 
#   "fuel_capacity", "number_of_decks", "guests", "water_capacity", 
#   "refits_num", "top_speed", "builder.supreme", "engine_make.unknown", 
#   "max_draught", "total_power", "displacement_tonnage", "range", 
#   "vip_rooms", "hull.Steel", "engine_make.Caterpillar", "hull_type.other_hull_type", 
#   "broker.burgessyachts", "builder.upper_standard", "classification.RINA", 
#   "builder.low_cost", "double_rooms", "broker.other_broker", "hull_type.Full.Displacement", 
#   "charter", "twin_rooms", "refits_bin", "yacht_subtype", "exterior_designer", 
#   "superstructure.other_superstructure", "superstructure.unknown", 
#   "broker.camperandnicholsons", "interior_designer", "superstructure.Aluminium", 
#   "engine_make.MTU", "classification.LR", "naval_architect", "broker.northropandjohnson", 
#   "hull_type.unknown", "engine_make.other_engine_make", "hull_number", 
#   "series_model_class", "classification.unknown", "hull.Aluminium", 
#   "classification.other_classification", "yacht_type.Motor.Yacht", 
#   "deck.Teak", "hull_type.Semi.Displacement", "builder.standard", 
#   "broker.edmiston", "classification.ABS", "deck.other_deck", "classification.BV", 
#   "broker.fraseryachts", "engine_make.MAN", "superstructure.GRP", 
#   "hull_type.Monohull", "hull_type.Deep.V...Planing", "hull.other_hull", 
#   "broker.oceanindependence", "broker.iyc", "broker.worthavenueyachts", 
#   "broker.denisonyachtsales", "master_rooms")





#### SELECCIÓN DEL MEJOR CONJUNTO DE VARIABLES ####

# Cargamos una función para validación cruzada repetida de
# regresiones, redes neuronales y random forest
source("./funciones/cruzadas avnnet y lin.R")
source("./funciones/cruzada rf continua.R")

# Los conjuntos tentativos son los siguientes
cvar1 <- c("gt", "length", "beam", "crew", "build_year")
cvar2 <- c("gt", "length", "beam", "crew", "build_year",
            "passenger_rooms", "number_of_decks", "fuel_capacity",
            "water_capacity", "refits_num")
cvar3 <- c("gt", "length", "beam", "crew", "build_year",
            "passenger_rooms", "number_of_decks", "fuel_capacity",
            "water_capacity", "refits_num", "guests", "builder.supreme",
            "top_speed", "max_draught", "displacement_tonnage")
cvar4 <- c("gt", "length", "beam", "crew", "build_year",
            "passenger_rooms", "number_of_decks", "fuel_capacity",
            "water_capacity", "refits_num", "guests", "builder.supreme",
            "top_speed", "max_draught", "displacement_tonnage",
            "engine_make.unknown", "total_power", "range", "vip_rooms",
            "interior_designer", "hull_type.other_hull_type", "hull.Steel",
            "builder.upper_standard", "hull_type.Full.Displacement",
            "broker.burgessyachts")
cvar5 <- c("gt", "length", "beam", "crew", "build_year",
            "passenger_rooms", "number_of_decks", "fuel_capacity",
            "water_capacity", "refits_num", "guests", "builder.supreme",
            "top_speed", "max_draught", "displacement_tonnage",
            "engine_make.unknown", "total_power", "range", "vip_rooms",
            "interior_designer", "hull_type.other_hull_type", "hull.Steel",
            "builder.upper_standard", "hull_type.Full.Displacement",
            "broker.burgessyachts", "engine_make.Caterpillar", "charter",
            "engine_make.other_engine_make", "double_rooms",
            "builder.low_cost", "refits_bin", "superstructure.unknown",
            "yacht_subtype", "twin_rooms", "exterior_designer",
            "classification.RINA", "engine_make.MTU", "hull_type.unknown",
            "hull_number", "naval_architect")
cvar6 <- barcos |>
  select(-price) |> 
  colnames()
cvar7 <- c("gt", "length", "build_year", "displacement_tonnage",
            "water_capacity", "total_power", "engine_make.Caterpillar",
            "hull_number")
cvar8 <- c("gt", "length", "build_year", "displacement_tonnage",
            "water_capacity", "total_power", "engine_make.Caterpillar",
            "hull_number", "beam", "builder.supreme", "engine_make.unknown",
            "vip_rooms")
cvar9 <- c("gt", "length", "build_year", "displacement_tonnage",
            "water_capacity", "total_power", "engine_make.Caterpillar",
            "hull_number", "beam", "builder.supreme", "engine_make.unknown",
            "vip_rooms", "crew", "number_of_decks", "fuel_capacity", "guests")

# Regresiones

medias1<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar1,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias1$modelo="C1"

medias2<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar2,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias2$modelo="C2"

medias3<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar3,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias3$modelo="C3"

medias4<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar4,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias4$modelo="C4"

medias5<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar5,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias5$modelo="C5"

medias6<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar6,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias6$modelo="C6"

medias7<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar7,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias7$modelo="C7"

medias8<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar8,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias8$modelo="C8"

medias9<-cruzadalin(data=archivo1,
                    vardep=vardep,
                    listconti=cvar9,
                    listclass=c(""),grupos=4,
                    sinicio=1234,repe=25)

medias9$modelo="C9"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,
              medias6,medias7,medias8,medias9)

par(cex.axis=0.8)
boxplot(data=union1,col="pink",error~modelo)



# Redes

medias1<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar1,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias1$modelo="C1"

medias2<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar2,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias2$modelo="C2"

medias3<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar3,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias3$modelo="C3"

medias4<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar4,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias4$modelo="C4"

medias5<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar5,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias5$modelo="C5"

medias6<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=c("gt", "length", "beam"),
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias6$modelo="C6-Nuevo"

medias7<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar7,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias7$modelo="C7"

medias8<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar8,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias8$modelo="C8"

medias9<-cruzadaavnnet(data=archivo1,
                       vardep=vardep,
                       listconti=cvar9,
                       listclass=c(""),grupos=4,
                       sinicio=1234,repe=25,
                       size=c(15),decay=c(0.01),
                       repeticiones=5,itera=100)

medias9$modelo="C9"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,
              medias6,medias7,medias8,medias9)

par(cex.axis=0.8)
boxplot(data=union1,col="pink",error~modelo)



# Random forests

medias1<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar1,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias1$modelo="C1"

medias2<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar2,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias2$modelo="C2"

medias3<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar3,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias3$modelo="C3"

medias4<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar4,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias4$modelo="C4"

medias5<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar5,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias5$modelo="C5"

medias6<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=c("gt", "length", "beam"),
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias6$modelo="C6-Nuevo"

medias7<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar7,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias7$modelo="C7"

medias8<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar8,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias8$modelo="C8"

medias9<-cruzadarf(data=archivo1,
                   vardep=vardep,
                   listconti=cvar9,
                   listclass=c(""),grupos=4,
                   sinicio=1234,repe=25,mtry=3,
                   ntree=150,nodesize=10,replace=TRUE,
                   sampsize=400)

medias9$modelo="C9"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,
              medias6,medias7,medias8,medias9)

par(cex.axis=0.8)
boxplot(data=union1,col="pink",error~modelo)


