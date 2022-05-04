library(tidyverse)
library(readxl)
library(lubridate)

theme_set(theme_bw())

# Import finance data ----
finanzas <- tibble(dir = list.files(path = "Data/", pattern = "^\\d{4}")) |> 
  mutate(dir = paste0("Data/", dir),
         year = as.numeric(str_extract(dir, "\\d+")),
         data = map(dir, read_excel, skip = 9, col_types = "text")) |> 
  unnest(data) 

interesting_descs <- c(
  "MANTENIMIENTO Y REPARACI?N DE VEH?CULOS",
  "REPUESTOS Y ACCESORIOS PARA MANTENIMIENTO Y REP. DE VEH?CULO",
  "OTROS MATERIALES, REPUESTOS Y ?TILES DIV. PARA MANT. Y REPA."
)

mant <- finanzas |> 
  filter(`Descripci?n` %in% interesting_descs) |> 
  select(-dir)

# Import SIC data----
timeseries_df <- readRDS("Data/timeseries_dataframe.rds")

# Correlation analysis ---- 
yearly_devengo <- mant |> 
  filter(year > 2016) |> 
  mutate(Devengo = as.numeric(Devengo)) |>
  group_by(year) |> 
  summarise(devengo = sum(Devengo), .groups = "drop")

yearly_sic <- timeseries_df[["monthly"]] |> 
  mutate(year = year(month)) |> 
  filter(year < 2022) |> 
  group_by(year) |> 
  summarise(total_sic = sum(total), .groups = "drop") 

corm <- left_join(yearly_devengo, yearly_sic, by = c("year")) |> 
  mutate(total_sic = if_else(is.na(total_sic), 0, total_sic),
         devengo_lag1 = lag(devengo, n = 1L, default = 0),
         devengo_lag2 = lag(devengo, n = 2L, default = 0),
         total_sic_lag1 = lag(total_sic, n = 1L, default = 0)) |> 
  select(-year) |> 
  relocate(devengo, devengo_lag1, devengo_lag2, total_sic, total_sic_lag1) |> 
  corrr::correlate(diagonal = 1) |> 
  corrr::shave(upper = FALSE)

corm <- corm |> 
  pivot_longer(
    cols = -term,
    names_to = "colname",
    values_to = "corr"
  ) |> 
  mutate(
    rowname = fct_inorder(term),
    colname = fct_inorder(colname),
    label = if_else(is.na(corr), "", sprintf("%1.2f", corr))
  )

ggplot(corm, aes(rowname, fct_rev(colname), fill = corr)) +
  geom_tile() +
  geom_text(aes(
    label = label,
    color = abs(corr) < .75
  )) +
  coord_fixed() +
  scale_color_manual(
    values = c("white", "black"),
    guide = "none"
  ) +
  scale_fill_distiller(
    palette = "BrBG", na.value = "white",
    direction = 1, limits = c(0, 1),
    name = "Correlación\nde Pearson"
  ) +
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_rect(color = NA, fill = NA),
        legend.position = c(.85, .8),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
