#########################################################################################
#########################################################################################
############### Análise dos determinantes da pobreza familiar no Brasil ############
#########################################################################################
#########################################################################################

# Importando pacotes ------------------------------------------------------

library(dplyr)
library(srvyr)
library(survey)
library(PNADcIBGE)

# Importando dados --------------------------------------------------------

variaveis <- c(
  "UF",
  "UPA",
  "V1008",
  "V1014",
  "V2007",
  "V2010",
  "VD3004",
  "V1022",
  "V2005",
  "V2009",
  "VD3005",
  "VD5008",
  "V3001",
  "VD4001",
  "VD4002",
  "VD4010",
  "V2001"
)

pnadc_raw <- PNADcIBGE::get_pnadc(
  year = 2024,
  interview = 1,
  vars = variaveis,
  labels = FALSE,
  design = FALSE
)

dplyr::glimpse(pnadc_raw)

## Muitas das variáveis estão armazenadas com o tipo `character`.
## Isso, posteriormente, pode acarretar em alguns erros de manipulação.
## Para evitar isto, transforma-se todas as variáveis em `numeric`

pnadc_raw <- pnadc_raw |>
  dplyr::mutate_if(is.character, as.numeric)

dplyr::glimpse(pnadc_raw)

# Manipulando a PNAD Contínua ---------------------------------------------

pnadc_manip <- pnadc_raw |>
  dplyr::mutate(
    chave_domicilio = factor(paste0(UPA, V1008, V1014)),
    nome_uf = factor(
      dplyr::case_when(
        UF == 11 ~ "Rondônia",
        UF == 12 ~ "Acre",
        UF == 13 ~ "Amazonas",
        UF == 14 ~ "Roraima",
        UF == 15 ~ "Pará",
        UF == 16 ~ "Amapá",
        UF == 17 ~ "Tocantins",
        UF == 21 ~ "Maranhão",
        UF == 22 ~ "Piauí",
        UF == 23 ~ "Ceará",
        UF == 24 ~ "Rio Grande do Norte",
        UF == 25 ~ "Paraíba",
        UF == 26 ~ "Pernambuco",
        UF == 27 ~ "Alagoas",
        UF == 28 ~ "Sergipe",
        UF == 29 ~ "Bahia",
        UF == 31 ~ "Minas Gerais",
        UF == 32 ~ "Espírito Santo",
        UF == 33 ~ "Rio de Janeiro",
        UF == 35 ~ "São Paulo",
        UF == 41 ~ "Paraná",
        UF == 42 ~ "Santa Catarina",
        UF == 43 ~ "Rio Grande do Sul",
        UF == 50 ~ "Mato Grosso do Sul",
        UF == 51 ~ "Mato Grosso",
        UF == 52 ~ "Goiás",
        UF == 53 ~ "Distrito Federal"
      )
    ),
    regiao = factor(
      dplyr::case_when(
        nome_uf %in%
          c(
            "Bahia",
            "Piauí",
            "Maranhão",
            "Rio Grande do Norte",
            "Sergipe",
            "Alagoas",
            "Ceará",
            "Paraíba",
            "Pernambuco"
          ) ~
          "Nordeste",
        nome_uf %in%
          c(
            "Tocantins",
            "Amazonas",
            "Pará",
            "Amapá",
            "Acre",
            "Roraima",
            "Rondônia"
          ) ~
          "Norte",
        nome_uf %in%
          c("São Paulo", "Minas Gerais", "Espírito Santo", "Rio de Janeiro") ~
          "Sudeste",
        nome_uf %in%
          c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul") ~
          "Centro-Oeste",
        nome_uf %in% c("Rio Grande do Sul", "Santa Catarina", "Paraná") ~ "Sul"
      )
    ),
    sexo = factor(
      dplyr::if_else(
        V2007 == 1,
        "Homem",
        "Mulher"
      )
    ),
    raca = factor(
      dplyr::case_when(
        V2010 == 1 ~ "Branco",
        V2010 == 2 ~ "Negro",
        V2010 == 3 ~ "Amarelo",
        V2010 == 4 ~ "Parda",
        V2010 == 5 ~ "Indígena"
      )
    ),
    nivel_instrucao = factor(
      dplyr::case_when(
        VD3004 == 1 ~ "Sem instrução e menos de 1 ano de estudo",
        VD3004 == 2 ~ "Fundamental incompleto ou equivalente",
        VD3004 == 3 ~ "Fundamental completo ou equivalente",
        VD3004 == 4 ~ "Médio incompleto ou equivalente",
        VD3004 == 5 ~ "Médio completo ou equivalente",
        VD3004 == 6 ~ "Superior incompleto ou equivalente",
        VD3004 == 7 ~ "Superior completo"
      ),
      levels = c(
        "Sem instrução e menos de 1 ano de estudo",
        "Fundamental incompleto ou equivalente",
        "Fundamental completo ou equivalente",
        "Médio incompleto ou equivalente",
        "Médio completo ou equivalente",
        "Superior incompleto ou equivalente",
        "Superior completo"
      ),
      ordered = TRUE
    ),
    localizacao_domicio = factor(
      dplyr::if_else(
        V1022 == 1,
        "Urbana",
        "Rural"
      ),
      levels = c(
        "Urbana",
        "Rural"
      )
    ),
    chefe_domicilio = factor(
      dplyr::if_else(
        V2005 == 1,
        "Sim",
        "Não"
      )
    ),
    idade = V2009,
    anos_estudo = VD3005,
    cond_forca = dplyr::if_else(
      VD4001 == 1,
      "Pessoas na força de trabalho",
      "Pessoas fora da força de trabalho"
    ),
    cond_ocup = dplyr::if_else(
      VD4002 == 1,
      "Pessoas ocupadas",
      "Pessoas desocupadas"
    ),
    num_membros_familia = V2001,
    trabalha_agricultura = dplyr::if_else(
      VD4010 == 1,
      "Trabalha na agricultura",
      "Não trabalha na agricultura"
    )
  )

dplyr::glimpse(pnadc_manip)

# Coloca a PNAD Contínua no formato amostral complexo ---------------------

pnadc_data <- pnadc_manip |>
  srvyr::as_survey_design(
    ids = UPA,
    strata = Estrato,
    weights = V1032,
    nest = TRUE
  )
