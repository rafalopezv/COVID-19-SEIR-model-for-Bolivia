# sobre: visualización
library(gganimate)
library(hrbrthemes)
library(tidyverse)

source("modelo.R")

Sys.setlocale(locale = "es_ES.UTF-8") # para tener fechas en español

# Vis 1: propagación con cuatro niveles de interacción
df %>% 
  filter(modelo %in% c(3, 10,20, 30)) %>% 
  mutate(
    modelo_etiqueta = as.factor(modelo_etiqueta)
  ) -> temp

# estática1: porcentaje población
(temp %>% 
  ggplot(aes(fecha, porcentaje_poblacion_infectada, color = modelo_etiqueta))  +
  geom_line(size = 2, alpha = 0.9) + 
  scale_x_date(date_labels = "%d %B", date_breaks = "1 month") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  hrbrthemes::theme_ipsum_rc(grid = "XY", axis_text_size = 12, axis_title_size = 12) +
  hrbrthemes::scale_color_ft() +
  theme(
    plot.title =  element_text(color = "#d8d8d8", size = 20, family = "Roboto Condensed"),
    plot.subtitle = element_text(color = "#d8d8d8", size = 25, family = "Roboto Condensed", face = "bold")
  ) + 
  facet_wrap(~fct_reorder(modelo_etiqueta, modelo, .desc = TRUE), ncol = 1) + 
  labs(
    title = "Efecto del aislamiento social en la expansión del COVID19",
    x = "Inicio computado desde el caso 0: 29 de Febrero",
    y = "Porcentaje población boliviana infectada"
  ) + 
  theme(legend.position = "none") +
  ggsave(here::here("img/estatico_1.jpg"), width = 10, height = 10)) 
  
# estática2:  población neta
(temp %>% 
    ggplot(aes(fecha, i, color = modelo_etiqueta))  +
    geom_line(size = 2, alpha = 0.9) + 
    scale_x_date(date_labels = "%d %B", date_breaks = "1 month") + 
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 12, axis_title_size = 12) +
    hrbrthemes::scale_color_ft() +
    theme(
      plot.title =  element_text(color = "#d8d8d8", size = 20, family = "Roboto Condensed"),
      plot.subtitle = element_text(color = "#d8d8d8", size = 25, family = "Roboto Condensed", face = "bold")
    ) + 
    facet_wrap(~fct_reorder(modelo_etiqueta, modelo, .desc = TRUE), ncol = 1) + 
    labs(
      title = "Efecto del aislamiento social en la expansión del COVID19",
      x = "Inicio computado desde el caso 0: 29 de Febrero",
      y = "Personas infectadas"
    ) + 
    theme(legend.position = "none") +
    ggsave(here::here("img/estatico_2.jpg"), width = 10, height = 10)) 


# animacion: porcentaje de población infectada
temp %>% 
    ggplot(aes(fecha, porcentaje_poblacion_infectada, color = modelo_etiqueta))  +
    geom_line(size = 2, alpha = 0.9) + 
    scale_x_date(date_labels = "%d %B", date_breaks = "1 month") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 20, axis_title_size = 20,base_size = 40,  
                            ) +
    hrbrthemes::scale_color_ft() +
    theme(
      plot.title =  element_text(color = "#d8d8d8", size = 30, family = "Roboto Condensed"),
      plot.subtitle = element_text(color = "#d8d8d8", size = 25, family = "Roboto Condensed", face = "bold"),
      strip.text = element_text(size = 22)
    ) + 
    facet_wrap(~fct_reorder(modelo_etiqueta, modelo, .desc = TRUE), ncol = 1) + 
    labs(
      title = "Efecto del aislamiento social en la expansión del COVID19",
      x = "Inicio computado desde el caso 0: 29 de Febrero",
      y = "Porcentaje población boliviana infectada"
    ) + 
    theme(legend.position = "none")  +
    transition_reveal(fecha) -> anim_1

anim_save(animation = anim_1, filename = "img/animacion_porcentaje.gif",
          width  = 1500, height = 1000,  fps = 35, duration = 2)  

# animacion: cantidad neta población infectada
temp %>% 
  ggplot(aes(fecha, i, color = modelo_etiqueta))  +
  geom_line(size = 2, alpha = 0.9) + 
  scale_x_date(date_labels = "%d %B", date_breaks = "1 month") + 
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 20, axis_title_size = 20,base_size = 40,  
  ) +
  hrbrthemes::scale_color_ft() +
  theme(
    plot.title =  element_text(color = "#d8d8d8", size = 30, family = "Roboto Condensed"),
    plot.subtitle = element_text(color = "#d8d8d8", size = 25, family = "Roboto Condensed", face = "bold"),
    strip.text = element_text(size = 22)
  ) + 
  facet_wrap(~fct_reorder(modelo_etiqueta, modelo, .desc = TRUE), ncol = 1) + 
  labs(
    title = "Efecto del aislamiento social en la expansión del COVID19",
    x = "Inicio computado desde el caso 0: 29 de Febrero",
    y = "Población boliviana infectada"
  ) + 
  theme(legend.position = "none")  +
  transition_reveal(fecha) -> anim_1

anim_save(animation = anim_1, filename = "img/animacion_poblacion.gif",
          width  = 1500, height = 1000,  fps = 35, duration = 2)  


# grafico de predicción modelo vs casos reales
dia_0_cuarentena %>% 
  filter(dia <= 22) %>% 
  select(-dia) %>% 
  gather(sigla, valor, -fecha) %>% 
  mutate(sigla = case_when(
    sigla == "i" ~ "Número de infectados estimados por el modelo",
    T ~ "Número oficial de infectados"
  ))  %>% 
  ggplot(aes(fecha, valor, color = sigla)) +
  geom_line(size = 2, alpha = 0.9) +
  scale_y_continuous(breaks = seq(0, 28, 2)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day") +
  hrbrthemes::theme_ipsum_rc(grid = "XY") +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(size = 20, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 13, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
  ) +
  labs(
    title = "Número oficial de contagiados en Bolivia vs Predicción del modelo SEIR",
    x = "",
    y = "número de personas contagiadas",
    caption = "Hasta el día de anuncio de la cuarentena total"
  ) +
  ggsave(here::here("img/estatico_3.jpg"), width = 10, height = 8)

# animacion de lo anterior
dia_0_cuarentena %>% 
  filter(dia <= 22) %>% 
  select(-dia) %>% 
  gather(sigla, valor, -fecha) %>% 
  mutate(sigla = case_when(
    sigla == "i" ~ "Número de infectados estimados por el modelo",
    T ~ "Número oficial de infectados"
  ))  %>% 
  ggplot(aes(fecha, valor, color = sigla)) +
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4, alpha = 0.8) +
  scale_y_continuous(breaks = seq(0, 28, 2)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day") +
  hrbrthemes::theme_ft_rc(grid = "XY", caption_size = 15, axis_text_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(color = "#d8d8d8", size = 24, family = "Roboto Condensed"),
    plot.subtitle =  element_text(color = "#d8d8d8", size = 22, family = "Roboto Condensed"),
    strip.text = element_text(size = 24),
    axis.title.y = element_text(hjust = 0.5, size = 16, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
  ) +
  labs(
    title = "Número oficial de contagiados en Bolivia vs Predicción del modelo SEIR",
    subtitle = "Desde fecha de ingreso a Bolivia de paciente '0' hasta declarataoria de cuarentena",
    x = "",
    y = "número de personas contagiadas",
    caption = "Valor RO = 6"
  ) +
  transition_reveal(fecha) -> anim_1

anim_save(animation = anim_1, filename = "img/animacion_pred_real_antes.gif",
          width  = 1200, height = 700,  fps = 30, duration = 15)

# prediccion vs realidad post cuarentena
serie_post %>% 
  gather(sigla, valor, -fecha) %>% 
  mutate(sigla = case_when(
    sigla == "numero_de_infectados_estimado_por_el_modelo_r0_2_4" ~ "Número de infectados estimados por el modelo",
    T ~ "Número oficial de infectados"
  ))  %>% 
  ggplot(aes(fecha, valor, color = sigla)) +
  geom_line(size = 2, alpha = 0.9) +
  scale_y_continuous(breaks = seq(30, 300, 30)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day") +
  hrbrthemes::theme_ipsum_rc(grid = "XY") +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(size = 20, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 13, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
  ) +
  labs(
    title = "Número oficial de contagiados en Bolivia vs Predicción del modelo SEIR",
    x = "",
    y = "número de personas contagiadas",
    caption = "Desde el inicio de la cuarentena total"
  ) +
  ggsave(here::here("img/estatico_7.jpg"), width = 10, height = 8)

# animacion del grafico anterior
serie_post %>% 
  gather(sigla, valor, -fecha) %>% 
  mutate(sigla = case_when(
    sigla == "numero_de_infectados_estimado_por_el_modelo_r0_2_4" ~ "Número de infectados estimados por el modelo",
    T ~ "Número oficial de infectados"
  ))  %>% 
  ggplot(aes(fecha, valor, color = sigla)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_point(size = 4, alpha = 0.9) +
  scale_y_continuous(breaks = seq(30, 300, 30)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day") +
  hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 18, caption_size = 17) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(color = "#d8d8d8",size = 25, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 15, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
  ) +
  labs(
    title = "Número oficial de contagiados en Bolivia vs Predicción del modelo SEIR",
    x = "",
    y = "número de personas contagiadas",
    caption = "Desde el inicio de la cuarentena total"
  ) +
  transition_reveal(fecha) -> anim_1
  
anim_save(animation = anim_1, filename = "img/animacion_pred_real_despues.gif",
          width  = 1200, height = 700,  fps = 30, duration = 17)


# grafico de hospitales pòr 1000 habitantes
tibble(
  Departamento = c("Pando", "Potosí", "Beni", "Oruro", "Cochabamba", "Promedio Bolivia", 
                   "Santa Cruz", "La Paz", "Tarija", "Chuquisaca"),
  n = c(0.83, 0.88, 0.95, 1.01, 1.25, 1.28, 1.29, 1.3, 1.39, 2.21),
) %>% 
  mutate(clase = case_when(
    Departamento == "Promedio Bolivia" ~ "promedio",
    T ~ "resto"
  ),
  num = 1:10) %>% 
  ggplot(aes(fct_reorder(Departamento, num, .desc = T), n, fill = clase)) + 
  geom_col(color = NA) + 
  geom_text(aes(label = n), hjust = -0.5, color = "black") +
  coord_flip() + 
  hrbrthemes::theme_ipsum_rc() + 
  theme(legend.position = "none") + 
  labs(
    title = "Número de camas por cada 1000 habitantes",
    x = "", 
    y = ""
  ) +
  ggsave("img/camas_habitantes.jpg", width = 10)

# mismo grafico anterior pero para web 
tibble(
  Departamento = c("Pando", "Potosí", "Beni", "Oruro", "Cochabamba", "Promedio Bolivia", 
                   "Santa Cruz", "La Paz", "Tarija", "Chuquisaca"),
  n = c(0.83, 0.88, 0.95, 1.01, 1.25, 1.28, 1.29, 1.3, 1.39, 2.21),
) %>% 
  mutate(clase = case_when(
    Departamento == "Promedio Bolivia" ~ "promedio",
    T ~ "resto"
  ),
  num = 1:10) %>% 
  ggplot(aes(fct_reorder(Departamento, num, .desc = T), n, fill = clase)) + 
  geom_col(color = NA, alpha = 0.6) + 
  geom_text(aes(label = n), hjust = -0.5, color = "white") +
  coord_flip() + 
  hrbrthemes::theme_ft_rc() + 
  theme(legend.position = "none") + 
  labs(
    title = "Número de camas por cada 1000 habitantes",
    x = "", 
    y = ""
  ) +
  ggsave("img/camas_habitantes_web.jpg", width = 10)



# si no se hubiese dictado la cuarentena vs resultados cuarentena
dia_0_cuarentena %>% 
  filter(dia != 40) %>% 
  select(-dia) %>% 
  gather(sigla, valor, -fecha) %>% 
  mutate(sigla = case_when(
    sigla == "i" ~ "Número de infectados estimados por el modelo",
    T ~ "Número oficial de infectados"
  ))  %>% 
  ggplot(aes(fecha, valor, color = sigla)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_vline(xintercept = as.Date("2020-03-22"), color = "black", 
             size = 0.7, alpha = 0.6, linetype = 3,) +
  scale_y_continuous(breaks = seq(0, 1100, 100)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day") +
  annotate("text", x = as.Date("2020-03-17"), y = 700, 
           label = "Declaración de cuarentena", color =  "black", 
           size = 5, family = "Roboto Condensed", fontface = 2) +
  geom_curve(aes(x = as.Date("2020-03-17"), y = 670, xend = as.Date("2020-03-22"), yend = 500), 
             colour = "black", 
             size = 0.2, 
             curvature = 0.2,
             arrow = arrow(length = unit(0.03, "npc"))) +
  hrbrthemes::theme_ipsum_rc(grid = "XY") +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(size = 20, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  labs(
    title = "Número de contagiados en Bolivia vs Predicción del modelo SEIR sin Cuarentena",
    x = "",
    y = "número de personas contagiadas"
  ) +
  ggsave(here::here("img/estatico_4.jpg"), width = 12, height = 8)

# animación grafico anterior
dia_0_cuarentena %>% 
  filter(dia != 40) %>% 
  select(-dia) %>% 
  gather(sigla, valor, -fecha) %>% 
  mutate(sigla = case_when(
    sigla == "i" ~ "Número de infectados estimados por el modelo",
    T ~ "Número oficial de infectados"
  ))  %>% 
  ggplot(aes(fecha, valor, color = sigla)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_point(size = 4) +
  geom_vline(xintercept = as.Date("2020-03-22"), color = "#d8d8d8", 
             size = 0.7, alpha = 0.6, linetype = 3,) +
  scale_y_continuous(breaks = seq(0, 1100, 100)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day") +
  annotate("text", x = as.Date("2020-03-17"), y = 700, 
           label = "Declaración de cuarentena", color =  "#d8d8d8", 
           size = 5, family = "Roboto Condensed", fontface = 2) +
  geom_curve(aes(x = as.Date("2020-03-17"), y = 670, xend = as.Date("2020-03-22"), yend = 500), 
             colour = "#d8d8d8", 
             size = 0.2, 
             curvature = 0.2,
             arrow = arrow(length = unit(0.03, "npc"))) +
  hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 15, axis_title_size = 17) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(color = "#d8d8d8", size = 25, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  labs(
    title = "Número de contagiados en Bolivia vs Predicción del modelo SEIR sin Cuarentena",
    x = "",
    y = "número de personas contagiadas"
  ) + 
  gganimate::transition_reveal(fecha) -> anim_1

anim_save(animation = anim_1, filename = "img/animacion_cuarentena_1.gif",
          width  = 1200, height = 700,  fps = 30, duration = 17)  

# gráfico de 4 curvas: 
dia_desde_antes_cuarentena %>% 
  select(-criticos, -severos, -mortalidad, -porcentaje_poblacion_infectada, -dia, -ro) %>% 
  gather(variable, valor, -fecha) %>% 
  mutate(variable = case_when(
    variable == "i" ~ "Infectados",
    variable == "e" ~ "Expuestos",
    variable == "s" ~ "Susceptibles",
    variable == "r" ~ "Removidos"
  )) %>% 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line(size = 2, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%d %B", date_breaks = "2 week") + 
  hrbrthemes::theme_ipsum_rc(grid = "XY") +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(size = 20, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  labs(
    title = "Simulación de la pandemia con la tendencia anterior a la declaración de la cuarentena",
    x = "",
    y = "Porcentaje de la pobalción boliviana"
  ) +
  ggsave(here::here("img/estatico_5.jpg"), width = 10, height = 8)

# animacion del grafico anterior
dia_desde_antes_cuarentena %>% 
  select(-criticos, -severos, -mortalidad, -porcentaje_poblacion_infectada, -dia, -ro) %>% 
  gather(variable, valor, -fecha) %>% 
  mutate(variable = case_when(
    variable == "i" ~ "Infectados",
    variable == "e" ~ "Expuestos",
    variable == "s" ~ "Susceptibles",
    variable == "r" ~ "Removidos"
  )) %>% 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line(size = 2, alpha = 0.9) +
  geom_point(size = 4, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%d %B", date_breaks = "2 week") + 
  hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(color = "#d8d8d8",  size = 25, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 19, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 19)
  ) +
  labs(
    title = "Simulación de la pandemia con la tendencia anterior a la declaración de la cuarentena",
    x = "",
    y = "Porcentaje de la pobalción boliviana"
  ) +
  transition_reveal(fecha) -> anim_1
  
anim_save(animation = anim_1, filename = "img/animacion_sin_cuarentena.gif.gif",
          width  = 1200, height = 700,  fps = 30, duration = 17)  

# grafico de 4 curvas: desde la cuarentena hasta 225 días despues
dia_desde_cuarentena %>% 
  select(fecha, s, e, i, r) %>% 
  gather(variable, valor, -fecha) %>% 
  mutate(variable = case_when(
    variable == "i" ~ "Infectados",
    variable == "e" ~ "Expuestos",
    variable == "s" ~ "Susceptibles",
    variable == "r" ~ "Removidos"
  )) %>% 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line(size = 2, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%d %B", date_breaks = "1 month") +
  hrbrthemes::theme_ipsum_rc(grid = "XY") +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(size = 20, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 13, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  labs(
    title = "Simulación de la pandemia con la tendencia después de la cuarentena",
    x = "",
    y = "Porcentaje de la población boliviana",
    caption = "Valor RO = 2.43"
  ) +
  ggsave(here::here("img/estatico_6.jpg"), width = 10, height = 8)

# animación del grafico anterior
dia_desde_cuarentena %>% 
  select(fecha, s, e, i, r) %>% 
  gather(variable, valor, -fecha) %>% 
  mutate(variable = case_when(
    variable == "i" ~ "Infectados",
    variable == "e" ~ "Expuestos",
    variable == "s" ~ "Susceptibles",
    variable == "r" ~ "Removidos"
  )) %>% 
  ggplot(aes(fecha, valor, color = variable)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_point(size = 4, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%d %B", date_breaks = "1 month") +
  hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 15, axis_title_size = 19, caption_size = 18) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(color = "#d8d8d8", size = 25, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, family = "Roboto Condensed Light", size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 18)
  ) +
  labs(
    title = "Simulación de la pandemia con la tendencia después de la cuarentena",
    x = "",
    y = "Porcentaje de la población boliviana",
    caption = "Valor RO = 2.43"
  ) +
  transition_reveal(fecha) -> anim_1

anim_save(animation = anim_1, filename = "img/animacion_con_cuarentena.gif",
          width  = 1200, height = 700,  fps = 35, duration = 15)  


# grafico de simulacion con dos ROs
dia_desde_antes_cuarentena %>% arrange(fecha) %>% 
  pull(fecha) %>% last() -> temp

dia_desde_cuarentena %>% arrange(fecha) %>% 
  pull(fecha) %>% last() -> temp_1

tibble(
  fecha = seq(as.Date(temp) + 1, as.Date(temp_1), 1)
) %>% 
  mutate(
    valor = rep(0, nrow(.)), 
    variable = rep("infectados", nrow(.)), 
    ro = rep("6", nrow(.)) 
  ) -> temp

bind_rows(dia_desde_antes_cuarentena, dia_desde_cuarentena) %>% 
  select(-criticos, -severos, -mortalidad, -porcentaje_poblacion_infectada, -dia, ro) %>% 
  gather(variable, valor, -fecha, -ro) %>% 
  filter(variable == "i") %>% 
  mutate(variable = case_when(
    variable == "i" ~ "Infectados"
  ), 
  ro = as.character(ro)) %>% 
  bind_rows(., temp) %>% 
  mutate(ro = case_when(
    ro == "6" ~ "Desde ingreso paciente '0 hasta declaración de cuarentena",
    T ~ "Desde declaración de cuarentena"
  )) %>% 
  ggplot(aes(fecha, valor, color = ro)) +
  geom_line(size = 2, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 0.28, 0.04)) +
  scale_x_date(date_labels = "%d %B", date_breaks = "1 month") + 
  hrbrthemes::theme_ipsum_rc(grid = "XY", axis_text_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(size = 19, family = "Roboto Condensed"),
    plot.subtitle =  element_text(size = 15, family = "Roboto Condensed"),
    strip.text = element_text(size = 14),
    axis.title.y = element_text(hjust = 0.5, size = 14, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  labs(
    title = "Porcentaje y momento de la población boliviana infectada bajo dos simulaciones",
    subtitle = "1: con tendencia hasta declaración de cuarentena; 2: con tendencia desde declaración de cuarentena",
    x = "",
    y = "Porcentaje de la población boliviana infectada", 
    caption = "ROs de 6 y 2.4"
  ) + 
  guides(color = guide_legend(reverse=TRUE)) + 
  ggsave("img/dos_modelos.jpg", width = 10, height = 8)


# animacion de evolución de infectados con RO 6 y RO 2.4

# extender el tiempo de una curva con valor 0 para que la animación concluya el mismo momento

dia_desde_antes_cuarentena %>% arrange(fecha) %>% 
  pull(fecha) %>% last() -> temp

dia_desde_cuarentena %>% arrange(fecha) %>% 
  pull(fecha) %>% last() -> temp_1

tibble(
  fecha = seq(as.Date(temp) + 1, as.Date(temp_1), 1)
) %>% 
  mutate(
    valor = rep(0, nrow(.)), 
    variable = rep("infectados", nrow(.)), 
    ro = rep("6", nrow(.)) 
  ) -> temp

bind_rows(dia_desde_antes_cuarentena, dia_desde_cuarentena) %>% 
  select(-criticos, -severos, -mortalidad, -porcentaje_poblacion_infectada, -dia, ro) %>% 
  gather(variable, valor, -fecha, -ro) %>% 
  filter(variable == "i") %>% 
  mutate(variable = case_when(
    variable == "i" ~ "Infectados"
  ), 
  ro = as.character(ro)) %>% 
  bind_rows(., temp) %>% 
  mutate(ro = case_when(
    ro == "6" ~ "Desde ingreso paciente '0 hasta declaración de cuarentena",
    T ~ "Desde declaración de cuarentena"
  )) %>% 
  ggplot(aes(fecha, valor, color = ro)) +
  geom_line(size = 2, alpha = 0.6) +
  geom_point(size = 4, alpha = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 0.28, 0.04)) +
  scale_x_date(date_labels = "%d %B", date_breaks = "1 month") + 
  hrbrthemes::theme_ft_rc(grid = "XY", axis_text_size = 15, caption_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title =  element_text(color = "#d8d8d8",  size = 25, family = "Roboto Condensed"),
    plot.subtitle =  element_text(color = "#d8d8d8",  size = 18, family = "Roboto Condensed"),
    strip.text = element_text(size = 22),
    axis.title.y = element_text(hjust = 0.5, size = 19, family = "Roboto Condensed Light"),
    legend.title = element_blank(),
    legend.text = element_text(size = 19)
  ) +
  labs(
    title = "Porcentaje y momento de la población boliviana infectada bajo dos simulaciones",
    subtitle = "1: con tendencia hasta declaración de cuarentena; 2: con tendencia desde declaración de cuarentena",
    x = "",
    y = "Porcentaje de la población boliviana infectada",
    caption = "ROs de 6 y 2.4"
  ) +
  guides(color = guide_legend(reverse=TRUE)) +
  transition_reveal(fecha) -> anim_1

anim_save(animation = anim_1, filename = "img/animacion_dos_modelos.gif",
          width  = 1200, height = 700,  fps = 30, duration = 17)  



