# Script para análise da pesquisa de ecologia humana - 2026.2
# Felipe Cordeiro, Felipe Fernandes, Jaderson Coriolano, Joyce Micaely, Letícia Gonçalves
# R 4.6.1

# Baseado no original de Alisson Peter Brito Dopona, que foi modificado por Ana Márcia

# Primeiros passos --------------------------------------------------------

## Bibliotecas ----
library(scales) # 1.4.0
library(tidyverse) # 2.0.0

## Importação dos dados ----
dados <- read.csv('dados.csv', header = TRUE, sep = ';', na.strings = '')

## Ajuste dos dados ----

### Transformação das colunas em fator ----
dados_fatorados <- dados |> mutate(
  across(everything(), as.factor)
)

# Verificando o N ---------------------------------------------------------

tabelas <- map(dados_fatorados, ~table(.x, useNA = 'no'))
tabelas

# Análises gráficas -------------------------------------------------------

## Estilo gráfico ----

estilo_principal <- theme_test() +
  theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
      legend.position = 'bottom',
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 11, color = 'black'),
      axis.title = element_text(size = 11)
  )

## Criação da função ----

graficos <- function(grupo_var, variavel, pergunta, rotulo_arquivo, rotulo_titulo) {
  sem_na <- dados_fatorados |> drop_na(all_of(grupo_var), all_of(variavel))
  contagens <- sem_na |> count(.data[[grupo_var]])
  lab_eixos <- paste0(contagens[[grupo_var]], '\n(n = ', contagens$n, ')')
  p <- sem_na |>
    count(.data[[grupo_var]], .data[[variavel]]) |>
    group_by(.data[[grupo_var]]) |>
    mutate(
      prop = n / sum(n),
      label = percent(prop, accuracy = 0.1)
    ) |>
    ggplot(aes(.data[[grupo_var]], prop, fill = .data[[variavel]])) +
    geom_bar(stat = 'identity', position = 'fill', width = 0.5) +
    geom_text(
      aes(label = label),
      position = position_fill(vjust = 0.5),
      size = 4
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = lab_eixos) +
    scale_fill_manual(values = c('#5b92e5', '#f7934c')) +
    estilo_principal +
    ggtitle(paste(pergunta, rotulo_titulo)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      x = '',
      y = '% das respostas',
      fill = ''
    )
  ggsave(
    paste0('./gráficos/', rotulo_arquivo, ' x ', variavel, '.png'),
    plot = p,
    height = 3,
    width = 4,
    dpi = 300,
    scale = 1.5
  )
}

## Execução da função ----

graficos('genero', 'chateacao', 'O que mais te chatearia?', 'Gênero', '(por gênero)')
graficos('genero', 'perdoar', 'O que seria mais difícil de perdoar?', 'Gênero', '(por gênero)')
graficos('genero', 'termino', 'O que aumentaria a probabilidade de término?', 'Gênero', '(por gênero)')
graficos('genero', 'traido', 'Já foi traído(a)?', 'Gênero', '(por gênero)')
graficos('genero', 'traiu', 'Já traiu?', 'Gênero', '(por gênero)')
graficos('sexo_nascimento', 'chateacao', 'O que mais te chatearia?', 'Sexo', '(por sexo)')
graficos('sexo_nascimento', 'perdoar', 'O que seria mais difícil de perdoar?', 'Sexo', '(por sexo)')
graficos('sexo_nascimento', 'termino', 'O que aumentaria a probabilidade de término?', 'Sexo', '(por sexo)')
graficos('sexo_nascimento', 'traido', 'Já foi traído(a)?', 'Sexo', '(por sexo)')
graficos('sexo_nascimento', 'traiu', 'Já traiu?', 'Sexo', '(por sexo)')

# Análises estatísticas ---------------------------------------------------

## Informações úteis ----

#' H0 = Não há associação estatística entre as variáveis (`p > 0.05`).
#' H1 = Há associação estatística entre as variáveis (`p <= 0.05`).
#' Resíduos ajustados com valor `> 1.96` indicam significância estatística.

## Análises ----

### Criação da função ----
analises <- function(grupo_var, variavel) {
  sem_na <- dados_fatorados |> drop_na(all_of(grupo_var), all_of(variavel))
  tabela <- table(sem_na[[variavel]], sem_na[[grupo_var]])
  print(tabela)
  resultado_qui <- tryCatch(
    {suppressWarnings(chisq.test(tabela))},
    error = function(e) {
      NULL
    }
  )
  tem_aviso_freq <- FALSE
  if (!is.null(resultado_qui)) {
    if (any(resultado_qui$expected < 5)) {
      tem_aviso_freq <- TRUE
    }
  }
  if (tem_aviso_freq) {
    resultado_fisher <- fisher.test(tabela, simulate.p.value = T, B = 10000)
    print(resultado_fisher)
  } else {
    print(resultado_qui)
    print(resultado_qui$stdres)
  }
}

### Execução da função ----

analises('genero', 'chateacao')
analises('genero', 'perdoar')
analises('genero', 'termino')
analises('genero', 'traido')
analises('genero', 'traiu')
analises('sexo_nascimento', 'chateacao')
analises('sexo_nascimento', 'perdoar')
analises('sexo_nascimento', 'termino')
analises('sexo_nascimento', 'traido')
analises('sexo_nascimento', 'traiu')
