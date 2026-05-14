# ════════════════════════════════════════════════════════════════
# PACOTES
# ════════════════════════════════════════════════════════════════

library(tidyverse)
library(readxl)
library(sf)
library(leaflet)

# ════════════════════════════════════════════════════════════════
# DIRETÓRIO DO PROJETO
# ════════════════════════════════════════════════════════════════

setwd("C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar")

# ════════════════════════════════════════════════════════════════
# LEITURA DOS DADOS IBGE
# ════════════════════════════════════════════════════════════════

# Estados (2023 e 2024)
dados_uf <- read_xlsx(
  "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/dados_Br_2023_2024.xlsx",
  sheet = "Estados"
)

# Brasil (sexo, raça, idade, educação, categoria emprego)
dados_brasil <- read_xlsx(
  "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/dados_Br_2023_2024.xlsx",
  sheet = "Brasil"
)


# ════════════════════════════════════════════════════════════════
# LEITURA DOS DADOS FAO
# ════════════════════════════════════════════════════════════════

#dados_latam <- read_xlsx(
#  "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/dados_latam.xlsx"
#)

dados_mundo <- read_xlsx("dados_mundo_FAO.xlsx")

# ════════════════════════════════════════════════════════════════
# PADRONIZA NOMES DOS PAÍSES
# ════════════════════════════════════════════════════════════════

#dados_latam <- dados_latam %>%
#  mutate(
#    Country = recode(
#      Country,
#      "Bolivia (Plurinational State of)" = "Bolivia"
#    )
#  )

# ════════════════════════════════════════════════════════════════
# SHAPEFILE DOS ESTADOS DO BRASIL
# ════════════════════════════════════════════════════════════════

geo_uf <- read_sf(
  "shapefiles/BR_UF_2025/BR_UF_2025.shp"
) %>%
  select(CD_UF, NM_UF, geometry) %>%
  mutate(
    CD_UF = as.numeric(CD_UF)
  )

# ════════════════════════════════════════════════════════════════
# JOIN ESTADOS + DADOS IBGE
# ════════════════════════════════════════════════════════════════

base_uf <- left_join(
  geo_uf,
  dados_uf,
  by = c("CD_UF" = "Codigo")
)

# ════════════════════════════════════════════════════════════════
# SHAPEFILE AMÉRICA LATINA
# ════════════════════════════════════════════════════════════════
#
#geo_latam <- read_sf(
#  "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/shapefiles/LATAM/ne_50m_admin_0_countries.shp"
#)

# ════════════════════════════════════════════════════════════════
# PAÍSES DA AMÉRICA LATINA
# ════════════════════════════════════════════════════════════════

#latam <- c(
#  "Argentina","Bolivia","Brazil","Chile","Colombia",
#  "Costa Rica","Cuba","Dominican Republic","Ecuador",
#  "El Salvador","Guatemala","Haiti","Honduras",
#  "Mexico","Nicaragua","Panama","Paraguay","Peru",
#  "Uruguay","Venezuela","Guyana","Suriname","Belize",
#  "Jamaica","Antigua and Barbuda"
#)

#geo_latam <- geo_latam %>%
#  filter(NAME %in% latam)

# ════════════════════════════════════════════════════════════════
# JOIN LATAM + DADOS FAO
# ════════════════════════════════════════════════════════════════

#base_latam <- left_join(
#  geo_latam,
#  dados_latam,
#  by = c("NAME" = "Country")
#)
# ════════════════════════════════════════════════════════════════
# PADRONIZA NOMES DOS PAÍSES (FAO → Natural Earth)
# ════════════════════════════════════════════════════════════════

dados_mundo <- dados_mundo %>%
  mutate(
    Country = recode(
      Country,
      
      # ── América ──────────────────────────────────────────────
      "Bolivia (Plurinational State of)"              = "Bolivia",
      "Venezuela (Bolivarian Republic of)"            = "Venezuela",
      
      # ── Europa / Ásia Central ─────────────────────────────────
      "Russian Federation"                            = "Russia",
      "Republic of Moldova"                           = "Moldova",
      "Czechia"                                       = "Czech Republic",
      "Türkiye"                                       = "Turkey",
      "North Macedonia"                               = "Macedonia",
      "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
      
      # ── África ───────────────────────────────────────────────
      "United Republic of Tanzania"                   = "Tanzania",
      "Democratic Republic of the Congo"              = "Democratic Republic of the Congo",
      "Republic of Congo"                             = "Republic of Congo",
      "Côte d'Ivoire"                                 = "Ivory Coast",
      "Eswatini"                                      = "Swaziland",
      "Cabo Verde"                                    = "Cape Verde",
      "Sao Tome and Principe"                         = "São Tomé and Príncipe",
      "Central African Republic"                      = "Central African Republic",
      "Equatorial Guinea"                             = "Equatorial Guinea",
      "South Sudan"                                   = "S. Sudan",
      
      # ── Ásia / Oceania ────────────────────────────────────────
      "Viet Nam"                                      = "Vietnam",
      "Iran (Islamic Republic of)"                    = "Iran",
      "Syrian Arab Republic"                          = "Syria",
      "Republic of Korea"                             = "South Korea",
      "Democratic People's Republic of Korea"         = "North Korea",
      "Lao People's Democratic Republic"              = "Laos",
      "Brunei Darussalam"                             = "Brunei",
      "Timor-Leste"                                   = "East Timor",
      "Micronesia (Federated States of)"              = "Micronesia",
      "Netherlands (Kingdom of the)"                  = "Netherlands",
      
      # ── Ilhas / Pequenos Estados ──────────────────────────────
      "Antigua and Barbuda"                           = "Antigua and Barb.",
      "Bosnia and Herzegovina"                        = "Bosnia and Herz.",
      "Dominican Republic"                            = "Dominican Rep.",
      "Saint Kitts and Nevis"                         = "St. Kitts and Nevis",
      "Saint Vincent and the Grenadines"              = "St. Vin. and Gren.",
      "Marshall Islands"                              = "Marshall Is.",
      "Solomon Islands"                               = "Solomon Is.",
      "Cook Islands"                                  = "Cook Is.",
      "Tokelau"                                       = "Tokelau",    # pode não existir no shapefile
      
      # China (regiões separadas no Natural Earth)
      "China, mainland"                               = "China",
      "China, Hong Kong SAR"                          = "Hong Kong",
      "China, Macao SAR"                              = "Macao",
      "China, Taiwan Province of"                     = "Taiwan"
    )
  )

# ════════════════════════════════════════════════════════════════
# SHAPEFILE MUNDO
# ════════════════════════════════════════════════════════════════

geo_mundo <- read_sf(
  "shapefiles/LATAM/ne_50m_admin_0_countries.shp"
)

# ── Diagnóstico: países ainda sem match ──────────────────────────
paises_fao   <- unique(dados_mundo$Country)
paises_shape <- unique(geo_mundo$NAME)
sem_match    <- paises_fao[!paises_fao %in% paises_shape]

if (length(sem_match) > 0) {
  cat("⚠️  Países ainda sem match no shapefile:\n")
  print(sem_match)
} else {
  cat("✅  Todos os países encontraram match!\n")
}

# ════════════════════════════════════════════════════════════════
# JOIN MUNDO + DADOS FAO
# ════════════════════════════════════════════════════════════════

base_mundo <- left_join(
  geo_mundo,
  dados_mundo,
  by = c("NAME" = "Country")
)

# ════════════════════════════════════════════════════════════════
# TESTE: FILTRAR ANO
# ════════════════════════════════════════════════════════════════

# IBGE 2024

# FAO 2023
#base_latam_2023 <- base_latam %>%
#  filter(Ano == 2023)

base_uf_2024    <- base_uf    %>% filter(Ano == 2024)
base_mundo_2023 <- base_mundo %>% filter(Ano == 2023)

# ════════════════════════════════════════════════════════════════
# TESTE MAPA BRASIL
# ════════════════════════════════════════════════════════════════

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = base_uf_2024$Ia
)

leaflet(base_uf_2024) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(Ia),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    label = ~paste0(
      NM_UF,
      "<br>",
      "Insegurança alimentar: ",
      Ia,
      "%"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~Ia,
    title = "Insegurança alimentar (%)"
  )

# ════════════════════════════════════════════════════════════════
# TESTE MAPA LATAM
# ════════════════════════════════════════════════════════════════

#pal2 <- colorNumeric(
#  palette = "Blues",
#  domain = base_latam_2023$Total
#)

#leaflet(base_latam_2023) %>%
#  addTiles() %>%
#  addPolygons(
#    fillColor = ~pal2(Total),
#    fillOpacity = 0.8,
#    color = "white",
#    weight = 1,
#    label = ~paste0(
#      NAME,
#      "<br>",
#      "Moderada ou grave: ",
#      Total,
#      "%"
#    )
#  ) %>%
#  addLegend(
#    pal = pal2,
#    values = ~Total,
#    title = "Moderada ou grave (%)"
#  )
######## SALVAR SHAPEFILES ###################
#IBGE
anos_ibge <- unique(base_uf$Ano)

for(a in anos_ibge){
  
  temp <- base_uf %>%
    filter(Ano == a)
  
  st_write(
    temp,
    paste0(
      "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/shapefiles/shapefiles_prontos/base_estados_",
      a,
      ".shp"
    ),
    delete_layer = TRUE
  )
  
}

# ════════════════════════════════════════════════════════════════
# SALVAR SHAPEFILES — MUNDO
# ════════════════════════════════════════════════════════════════

for (a in unique(na.omit(base_mundo$Ano))) {
  st_write(
    base_mundo %>% filter(Ano == a),
    paste0("shapefiles/shapefiles_prontos/base_mundo_", a, ".shp"),
    delete_layer = TRUE, quiet = TRUE
  )
}
#LATAM
#anos <- unique(base_latam$Ano)

#for(a in anos){
  
#  temp <- base_latam %>%
#    filter(Ano == a)
  
#  st_write(
#    temp,
#    paste0(
#      "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/shapefiles/shapefiles_prontos/base_latam_",
#      a,
#      ".shp"
#    ),
#    delete_layer = TRUE
#  )
  
#}

# ─────────────────────────────────────────────
# 1. LER SHAPEFILE DO BRASIL
# ─────────────────────────────────────────────

geo_brasil <- read_sf(
  "C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar/shapefiles/LATAM/ne_50m_admin_0_countries.shp"
) %>%
  
  filter(
    ADMIN == "Brazil"
  ) %>%
  
  select(
    ADMIN,
    geometry
  )


# ─────────────────────────────────────────────
# 4. CRIAR ID
# ─────────────────────────────────────────────
# necessário para o join

geo_brasil <- geo_brasil %>%
  
  mutate(
    id = 1
  )

dados_brasil <- dados_brasil %>%
  
  mutate(
    id = 1
  )

# ─────────────────────────────────────────────
# 5. JOIN
# ─────────────────────────────────────────────
# cada linha da tabela
# vira uma cópia do polígono

base_brasil <- left_join(
  
  dados_brasil,
  
  geo_brasil,
  
  by = "id"
  
) %>%
  
  st_as_sf()


# ─────────────────────────────────────────────
# 7. SALVAR SHAPEFILES
# ─────────────────────────────────────────────

anos_brasil <- unique(base_brasil$Ano)

for(a in anos_brasil){
  
  temp <- base_brasil %>%
    
    filter(
      Ano == a
    )
  
  st_write(
    
    temp,
    
    paste0(
      "shapefiles/shapefiles_prontos/base_brasil_",
      a,
      ".shp"
    ),
    
    delete_layer = TRUE,
    
    quiet = TRUE
    
  )
  
}
