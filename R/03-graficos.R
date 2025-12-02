# Importando pacotes ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(forcats)
library(srvyr)
library(survey)
library(geobr)
library(rlang)
library(showtext)

sysfonts::font_add_google(name = "Nunito")

sysfonts::font_add_google(name = "Lato")

sysfonts::font_add_google(name = "Montserrat")

sysfonts::font_add_google(name = "Fira Sans")

showtext::showtext_auto()

# Tema padrão

tema_padrao <- function(...) {
  ggplot2::theme_classic(...) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        colour = "black",
        size = 12,
        family = "Fira Sans"
      ),
      axis.text = ggplot2::element_text(
        colour = "black",
        size = 14,
        family = "Lato"
      ),
      axis.ticks = ggplot2::element_line(
        linewidth = 1,
        color = "black"
      ),
      axis.ticks.length = grid::unit(0.1, "cm"),
      legend.title = ggplot2::element_text(
        size = 12,
        family = "Nunito",
      ),
      legend.text = ggplot2::element_text(
        size = 10,
        family = "Montserrat"
      ),
      legend.position = "none",
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(
        size = 12,
        family = "Fira Sans"
      ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = "black",
        linewidth = 0.5
      )
    )
}

# Distribuição espacial da renda domiciliar per capita dos chefes de família

## Importa e manipula shapefile

br_shp <- geobr::read_state(code_state = "all", year = 2018)

dplyr::glimpse(br_shp)

## Calcula a renda média do chefe de família por UF

media_renda_cf_uf <- pnadc_data |>
  dplyr::filter(chefe_domicilio == "Sim") |>
  dplyr::group_by(UF, nome_uf) |>
  dplyr::summarise(
    renda_media = round(srvyr::survey_mean(VD5008, na.rm = TRUE), 2),
    .groups = "drop"
  )

## Faz o `join` entre as duas informações

br_shp_renda <- media_renda_cf_uf |>
  dplyr::left_join(
    br_shp,
    dplyr::join_by(UF == code_state)
  ) |>
  sf::st_as_sf()

## Gera o mapa

plot_renda_media_uf <- br_shp_renda |>
  dplyr::filter(name_state != "Distrito Federal") |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    ggplot2::aes(fill = renda_media),
    color = NA,
    linewidth = 0.15
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
    data = . %>%
      dplyr::filter(name_state != "Distrito Federal"),
    ggplot2::aes(label = name_state, hjust = "middle", vjust = "middle"),
    size = 5,
    family = "Fira Sans"
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
    labels = scales::dollar_format(
      prefix = "R$ ",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  ggplot2::labs(
    fill = "Renda em reais"
  )

plot(plot_renda_media_uf)

ggplot2::ggsave(
  filename = "Mapa renda domiciliar per capita por estado brasileiro.svg",
  plot = plot_renda_media_uf,
  device = "svg",
  path = "plots/",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
)

# Renda média por características do chefe de família

gerar_estat_agregadas <- function(.data, ...) {
  .data |>
    dplyr::filter(chefe_domicilio == "Sim") |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::drop_na() |>
    srvyr::mutate_if(is.numeric, round, 2)
}

## Renda média do chefe de família por sexo e raça

## Dados

tab_renda_sexo_raca <- gerar_estat_agregadas(.data = pnadc_data, sexo, raca)

## Plot

plot_tab_renda_sexo_raca <- tab_renda_sexo_raca |>
  ggplot2::ggplot(ggplot2::aes(
    x = forcats::fct_reorder(raca, renda_media),
    y = renda_media,
    fill = sexo
  )) +
  ggplot2::geom_col() +
  tema_padrao() +
  ggplot2::facet_wrap(facets = ~sexo) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::dollar(
        x = renda_media,
        prefix = "R$",
        accuracy = 0.01,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    vjust = -0.5,
    size = 4.5,
    family = "Fira Sans"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Homem" = "#65323EFF",
      "Mulher" = "#063B41FF"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(
      prefix = "R$ ",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  )

plot(plot_tab_renda_sexo_raca)

ggplot2::ggsave(
  filename = "Renda domiciliar per capita segmentada por sexo, Brasil.svg",
  plot = plot_tab_renda_sexo_raca,
  device = "svg",
  path = "plots/",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
)

# Renda média do chefe de família por nível de instrução

## Dados

tab_renda_niv_instrucao <- gerar_estat_agregadas(
  .data = pnadc_data,
  nivel_instrucao
)

## Plot

plot_renda_niv_instrucao <- tab_renda_niv_instrucao |>
  ggplot2::ggplot(ggplot2::aes(x = nivel_instrucao, y = renda_media)) +
  ggplot2::geom_col(fill = "#0E7C7BFF") +
  ggplot2::coord_flip() +
  tema_padrao() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::dollar(
        x = renda_media,
        prefix = "R$ ",
        accuracy = 0.01,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    hjust = "inward",
    size = 4,
    family = "Fira Sans"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(
      prefix = "R$ ",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  )

plot(plot_renda_niv_instrucao)

ggplot2::ggsave(
  filename = "Renda domiciliar per capita por nível de instrução, Brasil.svg",
  plot = plot_renda_niv_instrucao,
  device = "svg",
  path = "plots/",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
)

# Renda média do chefe de família por nível de instrução e sexo

## Dados

tab_renda_sexo_niv_instrucao <- gerar_estat_agregadas(
  .data = pnadc_data,
  nivel_instrucao,
  sexo
)

### Plot

plot_renda_sexo_niv_instrucao <- tab_renda_sexo_niv_instrucao |>
  ggplot2::ggplot(ggplot2::aes(
    x = nivel_instrucao,
    y = renda_media,
    fill = sexo
  )) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(facets = ~sexo) +
  ggplot2::coord_flip() +
  tema_padrao() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::dollar(
        x = renda_media,
        prefix = "R$ ",
        accuracy = 0.01,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    hjust = "inward",
    family = "Fira Sans"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(
      prefix = "R$ ",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c("Homem" = "#D62246FF", "Mulher" = "#0E7C7BFF")
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  )

plot(plot_renda_sexo_niv_instrucao)

ggplot2::ggsave(
  filename = "Renda domiciliar per capita por nível de instrução e sexo, Brasil.svg",
  plot = plot_renda_sexo_niv_instrucao,
  device = "svg",
  path = "plots/",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
)

# Renda domiciliar per capita por nível de instrução e região

## Dados

tab_renda_regiao_niv_instrucao <- gerar_estat_agregadas(
  .data = pnadc_data,
  nivel_instrucao,
  regiao
)

## Plot

plot_renda_regiao_niv_instrucao <- tab_renda_regiao_niv_instrucao |>
  ggplot2::ggplot(ggplot2::aes(
    x = nivel_instrucao,
    y = renda_media,
    fill = regiao
  )) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(facets = ~regiao) +
  ggplot2::coord_flip() +
  tema_padrao() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::dollar(
        x = renda_media,
        prefix = "R$ ",
        accuracy = 0.01,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    hjust = "inward",
    family = "Fira Sans"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Centro-Oeste" = "#0E7C7BFF",
      "Nordeste" = "#17BEBBFF",
      "Norte" = "#ACD39EFF",
      "Sudeste" = "#D62246FF",
      "Sul" = "#0076BBFF"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(
      prefix = "R$ ",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  )

plot(plot_renda_regiao_niv_instrucao)

ggplot2::ggsave(
  filename = "Renda domiciliar per capita por nível de instrução e região, Brasil.svg",
  plot = plot_renda_regiao_niv_instrucao,
  device = "svg",
  path = "plots/",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
)
