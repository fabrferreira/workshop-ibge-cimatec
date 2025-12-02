# Importando pacotes ------------------------------------------------------

library(dplyr)
library(srvyr)
library(survey)
library(rlang)

# Análise exploratória da amostra -----------------------------------------

## Total de indivíduos na amostra

n_individuos <- pnadc_data |>
  dplyr::summarise(n = srvyr::survey_total())

## Renda média na amostra

renda_media <- pnadc_data |>
  dplyr::summarise(renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE))

## Estatísticas descritivas da amostra

## Estatísticas descritivas do chefe de família na amostra

## Forma mais "trabalhosa"

tab_estatisticas <- pnadc_data |>
  dplyr::filter(chefe_domicilio == "Sim") |>
  dplyr::summarise(
    renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE),
    renda_mediana = srvyr::survey_median(VD5008, na.rm = TRUE),
    dp_renda = srvyr::survey_sd(VD5008, na.rm = TRUE),
    media_idade = srvyr::survey_mean(V2009, na.rm = TRUE),
    mediana_idade = srvyr::survey_median(V2009, na.rm = TRUE),
    dp_idade = srvyr::survey_sd(V2009, na.rm = TRUE),
    media_anos_estudo = srvyr::survey_mean(VD3005, na.rm = TRUE),
    mediana_anos_estudo = srvyr::survey_median(VD3005, na.rm = TRUE),
    dp_anos_estudo = srvyr::survey_sd(VD3005, na.rm = TRUE),
    media_membros = srvyr::survey_mean(num_membros_familia, na.rm = TRUE),
    mediana_membros = srvyr::survey_median(num_membros_familia, na.rm = TRUE),
    dp_membros = srvyr::survey_sd(num_membros_familia, na.rm = TRUE),
    n_obs = srvyr::survey_total(na.rm = TRUE),
    .groups = "rowwise"
  ) |>
  dplyr::mutate_if(is.numeric, round, 2)

## Mais código, porém possibilita maior nível de reprodutibilidade

pnadc_estat <- pnadc_data |>
  dplyr::filter(
    chefe_domicilio == "Sim"
  ) |>
  dplyr::summarise(
    dplyr::across(
      .cols = c(
        renda = VD5008,
        idade = V2009,
        anos_estudo = VD3005,
        num_membros_familia
      ),
      .fns = list(
        media = ~ srvyr::survey_mean(.x, na.rm = TRUE),
        mediana = ~ srvyr::survey_median(.x, na.rm = TRUE),
        dp = ~ srvyr::survey_sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}",
      .unpack = TRUE
    )
  )

## Manipula os dados

pnadc_estat_cf <- pnadc_estat |>
  tidyr::pivot_longer(
    cols = tidyr::everything(),
    names_to = "estatistica",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    tipo = dplyr::if_else(
      stringr::str_detect(estatistica, "_se$"),
      "erro_padrao",
      "valor"
    ),
    estatistica = stringr::str_remove(estatistica, "_coef$|__se$|_$")
  ) |>
  tidyr::separate(
    estatistica,
    into = c("variavel", "operacao"),
    sep = "_(?=[^_]+$)",
    remove = TRUE
  ) |>
  tidyr::pivot_wider(
    names_from = tipo,
    values_from = valor
  ) |>
  dplyr::mutate(
    variavel = dplyr::case_when(
      variavel == "renda" ~ "Renda domiciliar per capita",
      variavel == "idade" ~ "Idade",
      variavel == "num_membros_familia" ~ "Número membros família",
      variavel == "anos_estudo" ~ "Anos de estudo",
    ),
    operacao = dplyr::case_when(
      operacao == "media" ~ "Média",
      operacao == "mediana" ~ "Mediana",
      operacao == "dp" ~ "Desvio padrão",
      TRUE ~ operacao
    ),
    dplyr::across(
      .cols = dplyr::where(is.numeric),
      .fns = ~ round(.x, 2)
    )
  ) |>
  dplyr::select(
    `Variável` = variavel,
    `Operação` = operacao,
    `Valor` = valor,
    `Erro padrão` = erro_padrao
  )

# Em Excel

writexl::write_xlsx(
  pnadc_estat_cf,
  "tables/Estatísticas descritivas PNADC.xlsx"
)

# Em LaTeX

knitr::kable(
  pnadc_estat_cf,
  format = "latex"
)

## Função que permite otimizar o cálculo de estatísticas descritivas

gerar_estatistica <- function(.data, variable) {
  .data |>
    dplyr::filter(chefe_domicilio == "Sim") |>
    dplyr::group_by({{ variable }}) |>
    srvyr::survey_count() |>
    dplyr::ungroup() |>
    dplyr::mutate(percent = (n / sum(n)) * 100)
}

## Percentual de indivíduos por raça

composicao_raca <- gerar_estatistica(pnadc_data, raca)

## Percentual de indivíduos por sexo

composicao_sexo <- gerar_estatistica(pnadc_data, sexo)

## Percentual de indivíduos por tipo de zona onde reside (rural ou urbana)

composicao_dom_local <- gerar_estatistica(pnadc_data, localizacao_domicio)

## Percentual de indivíduos por região onde reside

composicao_regiao <- gerar_estatistica(pnadc_data, regiao)

## Percentual de indivíduos por situação de trabalho na agricultura

composicao_setor <- gerar_estatistica(pnadc_data, trabalha_agricultura)

## Renda domiciliar per capita média dos chefes de família

renda_media_cf <- pnadc_data |>
  dplyr::filter(chefe_domicilio == "Sim") |>
  dplyr::summarise(
    renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE)
  )
