# sobre: aplicación modelo SEIR a Bolivia: COVID19

library(deSolve)
library(tidyverse)
library(magrittr)

#-----------------------------------------
# preparacion y simulacion de la pandemia
#-----------------------------------------

options(scipen = 999) # remover notacion exponencial 

modelo_seir <- function (current_timepoint, state_values, parameters)
{
  # creacion de variables 
  s = state_values[1]         # susceptibles
  e =  state_values[2]        # expuestos 
  i =  state_values[3]        # infectados
  r =  state_values[4]        # removidos/recuperados
  
  with ( 
    as.list(parameters),      # para usar nombres de las variables dentro de los parametros
    {
      
      # sistema de ecuaciones diferenciales del modelo
      dS = (-beta * s * i)
      dE = (beta * s * i) - (delta * e)
      dI = (delta * e) - (gamma * i)
      dR = (gamma * i)
      
      
      # combinacion de resultados
      results <-  c(dS, dE, dI, dR)
      list(results)
    }
  )
}

# calculo de valores iniciales
w <-  11595000 - 1 - 30    # susceptibles : dato del ine del 7.4.20
x <-  1                    # infectados
y <-  0                    # removidos
z <-  30                   # expuestos

n <- w + x + y + z

valores_iniciales <-  c(s = w/n, e = x/n, i = y/n, r = z/n)

# calculo de duracion: 6 meses
duracion <-  seq(0, 180, 1)

# simulacion de la epidemia
parameter_list <- list()
ro <- numeric()
secuencia <- c(1:100) # variacion del ratio_contacto

for(a in secuencia) {
  ratio_contacto <- a
  probabilidad_transmision <- 0.04
  periodo_infeccion <-  7.5
  periodo_latencia <-  6
  valor_beta <-  ratio_contacto * probabilidad_transmision
  valor_gamma <-  1 / periodo_infeccion
  valor_delta <-  1 / periodo_latencia
  parameter_list[[a]] <-  c(beta = valor_beta, gamma = valor_gamma, delta = valor_delta)
  ro[a] <- valor_beta / valor_gamma
  cat(a, "\n")
}
rm(a)

# almacenaje de resultados
resultados <- list()

# variacion ro con rango 1:100
for(b in 1:length(parameter_list)) {
  resultados[[b]] <- lsoda(valores_iniciales, duracion, modelo_seir, parameter_list[[b]]) 
  resultados[[b]] %<>%  as.data.frame() 
  resultados[[b]]$ro <- ro[b]
  cat(b, "\n")
}
rm(b)

# añadir el nombre del modelo a cada base
for(a in secuencia) {resultados[[a]] %<>% mutate(modelo = secuencia[a])}
rm(a)

# compilado de base de datos
resultados %>% 
  bind_rows() %>% 
  gather(sigla, valor, -time, -ro, -modelo) %>% 
  mutate(
    valor = valor*n
  ) %>% 
  spread(sigla, valor) -> df

# creacion de variables auxiliares para visualizacion y otros
df %<>% 
  mutate(
    criticos = i * 0.05,
    severos = i * 0.14,
    mortalidad = i * 0.025,
    porcentaje_poblacion_infectada = i/n,
    modelo_etiqueta = paste0("Contacto con ", modelo, " personas"),
  ) %>% 
  rename(dia = time) %>% 
  arrange(modelo, dia)

# añadir fechas desde el caso 0 hasta el fin de la simulacion
temp <- as.Date("2020-02-29")
df$fecha <- seq(temp, temp + 180, 1) %>% rep(., 100) 

#  hallazgo de ro para predecir tendencia desde llegada de paciente 0 a cuarentena total
serie <- read_csv("input/serie_bolivia.csv") %>% janitor::clean_names()

# contagiados el día de decreto de cuarentena
serie %>% 
  filter(fecha == "2020-03-22") %>% 
  pull(numero_oficial_de_infectados)


serie_post <- read_csv("input/serie_bolivia_post_cuarentena.csv") %>% janitor::clean_names()

# contagiados el día de decreto de cuarentena
serie %>% 
  filter(fecha == "2020-03-22") %>% 
  pull(numero_oficial_de_infectados)

# verificar cual de las simulaciones predice un valor de 27
df %>% 
  filter(dia == 22) %>% 
  select(ro, dia, i) %>%
  filter(i > 26 & i < 28) # ro = 6
  
df %>% 
  filter(ro == 6) %>% 
  select(i, fecha) %>% 
  slice(1: nrow(serie)) %>% 
  merge(serie, .) -> dia_0_cuarentena


#---------------------------------------------------------
# con el ro hallado post cuarentena: evolución pandemia
#---------------------------------------------------------
w <-  11594163          # susceptibles : dato del ine del 7.4.20
x <-  27                # infectados
y <-  0                 # removidos
z <-  810               # expuestos

n <- w + x + y + z

valores_iniciales <-  c(s = w/n, e = x/n, i = y/n, r = z/n)

# ajuste de parametros
ratio_contacto <- 8.1 # parámetro hallado
probabilidad_transmision <- 0.04
periodo_infeccion <-  7.5
periodo_latencia <-  6
valor_beta <-  ratio_contacto * probabilidad_transmision
valor_gamma <-  1 / periodo_infeccion
valor_delta <-  1 / periodo_latencia
parametros <-  c(beta = valor_beta, gamma = valor_gamma, delta = valor_delta)
duracion <- seq(22, 250, 1) # 22 es inicio de cuarentena; parametro de 250 para ver evolución de curva

lsoda(valores_iniciales, duracion, modelo_seir, parametros) %>% 
  as.data.frame() %>% 
  mutate(
    criticos = i * 0.05,
    severos = i * 0.14,
    mortalidad = i * 0.025,
    porcentaje_poblacion_infectada = i/n,
    ro = valor_beta / valor_gamma,
    fecha = seq(as.Date("2020-03-23"), as.Date("2020-03-23") + (250 - 22), 1)
  ) %>% 
  rename(dia = time)  -> dia_desde_cuarentena

#---------------------------------------------------------
# con el ro hallado pre cuarentena: evolución pandemia
#---------------------------------------------------------
w <-  11594969          # susceptibles : dato del ine del 7.4.20
x <-  1                 # infectados
y <-  0                 # removidos
z <-  30               # expuestos

n <- w + x + y + z

# ajuste de parametros
ratio_contacto <- 20
probabilidad_transmision <- 0.04
periodo_infeccion <-  7.5
periodo_latencia <-  6
valor_beta <-  ratio_contacto * probabilidad_transmision
valor_gamma <-  1 / periodo_infeccion
valor_delta <-  1 / periodo_latencia
parametros <-  c(beta = valor_beta, gamma = valor_gamma, delta = valor_delta)
duracion <- seq(0, 180, 1)

lsoda(valores_iniciales, duracion, modelo_seir, parametros) %>% 
  as.data.frame() %>% 
  mutate(
    criticos = i * 0.05,
    severos = i * 0.14,
    mortalidad = i * 0.025,
    porcentaje_poblacion_infectada = i/n,
    ro = valor_beta / valor_gamma,
    fecha = seq(as.Date("2020-02-29"), as.Date("2020-02-29") + 180, 1)
  ) %>% 
  rename(dia = time)  -> dia_desde_antes_cuarentena 

# remover lo inservible
rm(valor_beta, ratio_contacto, valor_delta, valor_gamma, periodo_infeccion, valores_iniciales, temp,
   periodo_latencia, secuencia, duracion, probabilidad_transmision, w, x, y, z, parameter_list, resultados, n, ro, modelo_seir, 
   serie, parametros)


