# sobre: visualización
library(gganimate)
library(hrbrthemes)

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





 