# Importando pacotes ------------------------------------------------------

library(convey)
library(geobr)
library(dplyr)
library(ggplot2)
library(showtext)

sysfonts::font_add_google(name = "Nunito")

sysfonts::font_add_google(name = "Lato")

sysfonts::font_add_google(name = "Montserrat")

sysfonts::font_add_google(name = "Fira Sans")

showtext::showtext_auto()

# Salário-mínimo 2024: R$ 1.412
# As linhas de pobreza serão definidas em:
# 1/2 do SM do ano em questão: 706;
# 1/4 do SM do ano em questão para extrema pobreza: 353.

# Indicadores de Mercado de Trabalho --------------------------------------

# Taxa de desocupação no Brasil

tab_mercado_trabalho <- pnadc_data |>
  dplyr::filter(
    cond_forca == "Pessoas na força de trabalho"
  ) |>
  dplyr::group_by(cond_ocup) |>
  dplyr::summarise(
    n_pessoas_ocupadas = srvyr::survey_total()
  ) |>
  dplyr::mutate(
    taxa_perc = (n_pessoas_ocupadas / sum(n_pessoas_ocupadas, na.rm = TRUE)) *
      100
  )

# Taxa de desocupação por UF brasileira

tab_mercado_trabalho_uf <- pnadc_data |>
  dplyr::filter(
    cond_forca == "Pessoas na força de trabalho"
  ) |>
  dplyr::group_by(nome_uf, cond_ocup) |>
  dplyr::summarise(
    n_pessoas_ocupadas = srvyr::survey_total(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    taxa_perc = (n_pessoas_ocupadas / sum(n_pessoas_ocupadas, na.rm = TRUE)) *
      100,
    .by = nome_uf
  )

# Preparando a PNADC ------------------------------------------------------

## Adiciona a `PNAD Contínua` à função `convey_prep()`

pnadc_convey <- pnadc_data |>
  dplyr::filter(chefe_domicilio == "Sim") |>
  convey::convey_prep()

## A função abaixo nos permite otimizar o fluxo de trabalho ao calcular
## o índice FGT, converter os resultados em um tibble e exportar como um
## arquivo .xlsx

calcular_fgt <- function(linha_pobreza, variavel, tipo_fgt, diretorio) {
  fgt_bruto <- survey::svyby(
    ~VD5008,
    variavel,
    pnadc_convey,
    convey::svyfgt,
    g = tipo_fgt,
    abs_thresh = {{ linha_pobreza }},
    type_thresh = "abs",
    na.rm = TRUE
  ) |>
    dplyr::as_tibble()

  writexl::write_xlsx(x = fgt_bruto, path = diretorio)

  return(fgt_bruto)
}

## Índice FGT por sexo

fgt_por_sexo <- calcular_fgt(
  linha_pobreza = 706,
  variavel = ~sexo,
  tipo_fgt = 2,
  diretorio = "tables/fgt_sexo.xlsx"
)

## Índice FGT por UF

fgt_por_estado <- calcular_fgt(
  variavel = ~nome_uf,
  tipo_fgt = 2,
  diretorio = "tables/fgt_uf_estado_sm.xlsx"
)

# Gerando gráficos --------------------------------------------------------

br_shp <- geobr::read_state(code_state = "all", year = 2018)

br_shp <- br_shp |>
  dplyr::mutate(
    name_state = stringr::str_replace_all(
      string = name_state,
      pattern = stringr::regex("\\b(Da|De|Do)\\b"),
      replacement = \(match) stringr::str_to_lower(match)
    )
  )

plt_indice_fgt_uf <- fgt_por_estado |>
  dplyr::left_join(
    br_shp,
    dplyr::join_by(nome_uf == name_state)
  ) |>
  sf::st_as_sf() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    ggplot2::aes(fill = VD5008),
    color = "black",
    size = 0.15
  ) +
  ggthemes::theme_map() +
  ggplot2::theme(
    legend.position = "left",
    legend.justification = c("right", "bottom"),
    legend.title = ggplot2::element_text(
      vjust = 0.5,
      hjust = 0.5,
      size = 12,
      family = "Fira Sans"
    ),
    legend.text = ggplot2::element_text(size = 10, family = "Nunito"),
    legend.box.background = ggplot2::element_rect(
      color = "black",
      linewidth = 0.5
    ),
    legend.box.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1),
    legend.box.just = "right"
  ) +
  ggplot2::geom_sf_text(
    ggplot2::aes(label = nome_uf, hjust = 0.5, vjust = 0.5),
    size = 4,
    check_overlap = TRUE,
    family = "Montserrat"
  ) +
  ggplot2::scale_fill_gradientn(
    colours = c(
      "#F7FBFFFF",
      "#C6DBEFFF",
      "#9ECAE1FF",
      "#4292C6FF",
      "#2171B5FF"
    ),
    na.value = "#A9A9A9",
    labels = scales::percent_format(
      accuracy = 1,
      big.mark = ".",
      decimal.mark = ","
    ),
    breaks = c(0, 0.05, 0.10, 0.15, 0.20),
    limits = c(0, 0.20)
  ) +
  ggplot2::labs(
    fill = "FGT em (%)"
  )

plot(plt_indice_fgt_uf)

ggplot2::ggsave(
  filename = "Mapa índice FGT por estado brasileiro.svg",
  plot = plt_indice_fgt_uf,
  device = "svg",
  path = "plots/",
  width = 12,
  height = 7,
  dpi = 150,
  units = "in"
)
