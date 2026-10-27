# Descrição do script de análise de dados (ecologia humana)

O script apresentado realiza uma análise descritiva, visual e inferencial sobre dados categóricos coletados em uma pesquisa de ecologia humana, comparando respostas comportamentais em função do **gênero** e do **sexo de nascimento**.

---

## 1. Estrutura inicial e preparação dos dados

1. **Carregamento de bibliotecas**:
   * `tidyverse` (em especial `dplyr` e `ggplot2`): Manipulação de dados e visualização gráfica.
   * `scales`: Formatação numérica (exibição de porcentagens nos eixos e rótulos).
2. **Importação e tipagem**:
   * O arquivo `dados.csv` é lido com separador `,` e campos vazios são convertidos em `NA`.
   * Todas as variáveis são transformadas em **fatores** (`as.factor`) por meio de `mutate(across(everything(), as.factor))`, já que se trata de uma pesquisa com respostas categóricas/qualitativas.
3. **Verificação de frequências**:
   * Através de `map(dados_fatorados, ~table(.x, useNA = 'no'))`, o script imprime a distribuição de frequências absolutas de cada variável, excluindo valores nulos, permitindo uma checagem rápida do tamanho amostral (N).

---

## 2. Funções personalizadas

O script automatiza tarefas repetitivas por meio de duas funções centrais: Uma voltada para visualização (`graficos`) e outra para inferência estatística (`analises`).

---

### Função 1: `graficos()` (visualização descritiva)

* **Objetivo**: Padronizar e salvar gráficos de barras 100% empilhadas para pares de variáveis.
* **Parâmetros**:
  * `grupo_var`: Variável independente/agrupadora (ex.: `'genero'`, `'sexo_nascimento'`).
  * `variavel`: Variável dependente/resposta (ex.: `'chateacao'`, `'traiu'`).
  * `pergunta`: Enunciado da questão para compor o título.
  * `rotulo_arquivo`: Nome base do arquivo de saída.
  * `rotulo_titulo`: Complemento do título (ex.: `"(por gênero)"`).
* **Funcionamento interno**:
  1. **Filtragem**: Remove linhas com dados ausentes (`NA`) nas duas variáveis de interesse com `drop_na()`.
  2. **Tamanho amostral no eixo**: Calcula o total de respondentes por grupo e formata o rótulo do eixo X com o tamanho amostral (ex.: `Mulher \n (n = 50)`).
  3. **Cálculo de proporções**: Agrupa os dados e calcula a frequência relativa (`prop`) de cada resposta, formatando o valor como porcentagem (`label`).
  4. **Construção do gráfico (`ggplot2`)**:
     * Gera barras empilhadas normalizadas (`position = 'fill'`).
     * Adiciona o texto percentual no centro de cada segmento da barra (`geom_text`).
     * Aplica uma paleta de duas cores (`#5b92e5` e `#f7934c`).
  5. **Exportação**: Salva a figura em alta resolução (300 DPI) na pasta `./gráficos/` usando `ggsave()`.

---

### Função 2: `analises()` (inferência estatística e tratamento condicional)

Esta função automatiza a testagem de hipóteses para tabelas de contingência e implementa uma lógica adaptativa baseada nos pressupostos dos testes estatísticos.

#### Objetivo e Hipóteses
* **Hipótese nula (H0)**: Não há associação entre as duas variáveis categóricas (p > 0,05).
* **Hipótese alternativa (H1)**: Há associação estatisticamente significativa entre as variáveis (p <= 0,05).

---

#### Detalhamento passo a passo da lógica

```r
analises <- function(grupo_var, variavel) {
  # 1. Limpeza e Tabulação
  sem_na <- dados_fatorados |> drop_na(all_of(grupo_var), all_of(variavel))
  tabela <- table(sem_na[[variavel]], sem_na[[grupo_var]])
  print(tabela)
  
  # 2. Tentativa do Teste de Qui-Quadrado com tratamento de erro
  resultado_qui <- tryCatch(
    {suppressWarnings(chisq.test(tabela))},
    error = function(e) { NULL }
  )
  
  # 3. Verificação do pressuposto de frequências esperadas
  tem_aviso_freq <- FALSE
  if (!is.null(resultado_qui)) {
    if (any(resultado_qui$expected < 5)) {
      tem_aviso_freq <- TRUE
    }
  }
  
  # 4. Estrutura de Decisão (if / else)
  if (tem_aviso_freq) {
    resultado_fisher <- fisher.test(tabela, simulate.p.value = T, B = 10000)
    print(resultado_fisher)
  } else {
    print(resultado_qui)
    print(resultado_qui$stdres)
  }
}
```

---

#### Explicação detalhada do tratamento e do `if / else`

O cerne dessa função é escolher dinamicamente entre o **teste qui-quadrado de independência** e o **teste exato de Fisher**, dependendo do atendimento ao pressuposto de frequências esperadas (critério de Cochran):

1. **Uso de `tryCatch()`**:
   * O teste de qui-quadrado (`chisq.test`) pode falhar criticamente caso a tabela seja degenerada (por exemplo, uma linha ou coluna inteira com zeros).
   * O bloco `tryCatch` tenta rodar o teste silenciando avisos (`suppressWarnings`) e, caso ocorra um erro fatal, retorna `NULL` em vez de interromper a execução do script.

2. **Avaliação das frequências esperadas (`resultado_qui$expected < 5`)**:
   * O teste qui-quadrado é uma aproximação assintótica. Um de seus pressupostos clássicos exige que nenhuma célula (ou não mais de 20% das células) tenha valor esperado menor do que 5.
   * O código checa `any(resultado_qui$expected < 5)`. Se ao menos uma casela esperada tiver valor inferior a 5, a variável lógica `tem_aviso_freq` recebe `TRUE`.

3. **O desvio condicional (`if / else`)**:

   * **Ramo `if (tem_aviso_freq)` — Quando o pressuposto é violado**:
     * Como a aproximação pelo Qui-Quadrado se torna imprecisa com frequências esperadas baixas, o código recorre ao **Teste Exato de Fisher** (`fisher.test`).
     * Devido ao fato de que tabelas maiores que 2x2 exigem alto custo computacional para o cálculo exato do fatorial, a função usa `simulate.p.value = TRUE` com B = 10000 replicações de Monte Carlo. Isso garante robustez estatística sem travar o processamento.
     * Imprime o resultado do teste de Fisher simulado.

   * **Ramo `else` — Quando o pressuposto é plenamente atendido**:
     * Imprime o resultado do **teste qui-quadrado** padrão (`resultado_qui`).
     * Imprime a matriz de **resíduos padronizados ajustados** (`resultado_qui$stdres`):
       * Os resíduos ajustados funcionam como uma análise *post-hoc*.
       * Valores com módulo maior que **1,96** indicam que a contagem observada naquela célula específica se desvia da esperada ao nível de significância de 5% (alpha = 0,05), permitindo identificar exatamente quais combinações de categorias motivaram a associação detectada.

---

## 3. Síntese da Execução

Ao final do script, ambas as funções são executadas em bloco para todas as combinações de interesse:
* **Cruzamentos**: Ambas as variáveis explicativas (`genero` e `sexo_nascimento`) contra as cinco variáveis de desfecho (`chateacao`, `perdoar`, `termino`, `traido`, `traiu`).
* **Saída final**: Uma coleção de 10 figuras prontas para publicação na pasta `./gráficos/` e os relatórios estatísticos correspondentes impressos sequencialmente no console do R.

---

*Texto gerado por IA*
