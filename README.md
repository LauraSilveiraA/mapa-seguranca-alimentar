# Segurança alimentar — Brasil e mundo

Mapa interativo desenvolvido em R e Leaflet para visualização de indicadores de segurança e insegurança alimentar no Brasil e no mundo.

## Acesso ao mapa

O mapa pode ser acessado em:

https://laurasilveiraa.github.io/mapa-seguranca-alimentar/

---

## Objetivo

O projeto apresenta indicadores de segurança alimentar para:

* Brasil
* estados brasileiros
* países do mundo

com dados provenientes do:

* PNAD Contínua — IBGE
* Food Insecurity Experience Scale (FIES) — FAO

---

## Indicadores

### IBGE

* Segurança alimentar
* Insegurança alimentar
* Insegurança alimentar leve
* Insegurança alimentar moderada
* Insegurança alimentar grave

### FAO

* Prevalência de insegurança alimentar moderada ou grave

Os dados da FAO correspondem a médias móveis de três anos.

---

## Grupos 

### Brasil

Os dados brasileiros permitem visualização por:

* Total
* Raça/cor
* Sexo
* Educação
* Idade
* Profissão (posição na ocupação e categoria do emprego no trabalho)

---

## Metodologia

Os indicadores foram calculados considerando o total de indivíduos dentro de cada grupo social.

Exemplo:

A porcentagem de pessoas pardas em segurança alimentar corresponde ao total de pessoas pardas em segurança alimentar dividido pelo total de pessoas pardas.

A pesquisa do IBGE é realizada por domicílio, considerando a situação do responsável pelo domicílio.

A categoria idade foi calculada por morador, considerando a idade individual das pessoas pesquisadas.

---

## Estrutura do repositório

### `codigos/`

Scripts em R utilizados para:

* tratamento dos dados
* criação do mapa interativo

### `dados/`

Bases de dados utilizadas no projeto.

---

## Tecnologias utilizadas

* R
* Leaflet
* sf
* dplyr
* htmlwidgets
* JavaScript
* Bootstrap

---

## Elaboração

Aluna Laura Silveira Alves para a disciplina de Sociobiodiversidade, soberania e segurança alimentar e nutricional da professora Gabriela Coelho de Souza.
