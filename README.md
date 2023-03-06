# MA-Analisis-Politico-y-Electoral-UC3M-

```{r, warning=F, message=F, echo=FALSE}
library(tidyverse)
library(googlesheets4)
library(janitor)
library(hrbrthemes)
library(ggthemes)
library(ggplot2)
remotes::install_github("tidyverse/ggplot2")
```

# **1. Cargamos dataset**
```{r,  warning=F, message=F}
url <- "https://docs.google.com/spreadsheets/d/1xMbcOz-ldPW-82MAmRzdpnr_bkc0e1LYianLKwpor5E/edit#gid=362203764"
riqueza_df <- googlesheets4::read_sheet(url, sheet= "riqueza_neta") %>% janitor::clean_names()
```

**Para el gráfico en R usaré theme_fivethirtyeight, pero cambiando el fondo gris por uno blanco**
```{r}
theme_fivethirtyeight <- function(base_size = 11, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA))
}
```

# **2. Creamos gráfico en R**
```{r, warning=FALSE}
ejercicio1 <- riqueza_df%>%
  ggplot() + 
  
  geom_line(aes(year, entre_35_y_44), size=1.5, color="darkorange") + 
  geom_line(aes(year, entre_65_y_74), size=1.5, color="darkmagenta") + 
  
  geom_point(aes(year, entre_35_y_44), size=4, color="darkorange", alpha=0.5)+
  geom_point(aes(year,  entre_65_y_74), size=4, color="darkmagenta", alpha=0.5) +
  
  geom_hline(yintercept = 0, alpha=0.7) +
  geom_ribbon(aes(x=year, ymin=entre_35_y_44, ymax=entre_65_y_74), fill="bisque1", alpha=.4) +

  scale_x_continuous(breaks= seq(2004,2030,2), minor_breaks = NULL) + 
  scale_y_continuous(limits=c(0,NA), label = function(x) paste0(x,'€')) + 
  
  
  labs(
    title = "¿Cómo ha evolucionado la riqueza para\nlos diferentes segmentos de edad?",
    subtitle = "Patrimonio en miles de € para aquellos
    <span style = 'color:darkorange2'>**entre 34 y 45**</span>, o 
    <span style = 'color:darkmagenta;'>**entre 64 y 75**</span>",
    caption="María Hdez. MAPE (2022/2023)\nVisualización y presentación de datos", 
    x = "",
    y = ""
  ) + 
  
  annotate('text', label='La diferencia entre generaciones\nno superaba los\n20 mil € en 2002', 
         x=2006, y=95, hjust=0.5, vjust=1, 
         size=3.5, color='black', fontface = "bold") + 
  
  annotate('curve', x=2004, y=70, xend=2002, yend=105, 
         color='black', curvature=-0.5, 
         arrow = arrow(length = unit(0.03, "npc"))) + 
  
  annotate('text', label='Pero ha ido ascendiendo hasta\nlos 100 mil € en 2020', 
         x=2017, y=150, hjust=0.5, vjust=1, 
         size=3.5, color='black', fontface = "bold") +
  
  annotate('curve', x=2019, y=125, xend=2020, yend=75, 
         color='black', curvature=-0.5, 
         arrow = arrow(length = unit(0.03, "npc")))  +

    theme_fivethirtyeight()


ejercicio1 + theme(plot.title = element_text(face = "bold", colour = "black", size=13),
                   plot.subtitle = ggtext::element_markdown(), 
                   panel.grid.major = element_line(color = 'gray', linetype = 'dotted'), 
                   plot.caption = element_text(face="bold", size=9))
```
