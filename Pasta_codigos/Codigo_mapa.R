# ════════════════════════════════════════════════════════════════
# MAPA INTERATIVO — SEGURANÇA ALIMENTAR
# IBGE + FAO | Brasil + UFs + Mundo
# ════════════════════════════════════════════════════════════════

library(sf)
library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(readxl)

# ===============================================================
# 1. PASTA
# ===============================================================

setwd("C:/Users/laura/OneDrive/Documentos/mapa_segu_alimentar")

# ===============================================================
# 2. SHAPEFILES BRASIL
# ===============================================================

brasil_2023 <- st_read("shapefiles/shapefiles_prontos/base_brasil_2023.shp", quiet = TRUE)
brasil_2024 <- st_read("shapefiles/shapefiles_prontos/base_brasil_2024.shp", quiet = TRUE)
uf_2023     <- st_read("shapefiles/shapefiles_prontos/base_estados_2023.shp", quiet = TRUE)
uf_2024     <- st_read("shapefiles/shapefiles_prontos/base_estados_2024.shp", quiet = TRUE)

# ===============================================================
# 3. SHAPEFILES MUNDO (FAO)
# ===============================================================

anos_fao <- 2016:2023
mundo    <- list()

for (a in anos_fao) {
  mundo[[as.character(a)]] <- st_read(
    paste0("shapefiles/shapefiles_prontos/base_mundo_", a, ".shp"),
    quiet = TRUE
  )
}

# ===============================================================
# 4. DADOS BRASIL
# ===============================================================

geo_brasil <- brasil_2024 %>% slice(1)

base_brasil <- bind_rows(
  brasil_2023 %>% st_drop_geometry(),
  brasil_2024 %>% st_drop_geometry()
)

json_brasil <- toJSON(base_brasil, dataframe = "rows", auto_unbox = TRUE)

# ===============================================================
# 5. PALETA
# ===============================================================

pal <- colorBin(
  palette  = c(
  "#1B5E20", 
  "#2E7D32",  
  "#43A047",  
  "#7CB342",  
  "#C0CA33",  
  "#FDD835",  
  "#FFB300",  
  "#FB8C00"   
),
  domain   = c(0, 100),
  bins     = c(0, 10, 20, 30, 40, 50, 60, 80, 100),
  na.color = "#cccccc"
)

# ===============================================================
# 6. MAPA BASE
# ===============================================================

mapa <- leaflet(
  options = leafletOptions(
    attributionControl = FALSE,
    minZoom = 2,
    maxZoom = 8,
    worldCopyJump = TRUE   # mapa "redondo"
  )
) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -52, lat = -14, zoom = 4)  # inicia no Brasil

# ===============================================================
# 7. CAMADAS UFs
# ===============================================================

for (a in c(2023, 2024)) {
  
  dados <- if (a == 2023) uf_2023 else uf_2024
  
  tooltip <- paste0(
    "<div style='font-family:Arial;font-size:13px;'>",
    "<strong>", dados$NM_UF, "</strong><br><br>",
    "Segurança alimentar: <b>",    round(dados$Seg,  1), "%</b><br>",
    "Insegurança alimentar: <b>",  round(dados$Ia,   1), "%</b><br><br>",
    "Leve: ",     round(dados$Leve, 1), "%<br>",
    "Moderada: ", round(dados$Mode, 1), "%<br>",
    "Grave: ",    round(dados$Grave,1), "%",
    "</div>"
  )
  
  mapa <- mapa %>%
    addPolygons(
      data         = dados,
      fillColor    = ~pal(Ia),
      fillOpacity  = 0.8,
      color        = "white",
      weight       = 1,
      smoothFactor = 0.5,
      # label: aparece ao passar o mouse e fica fixo ao clicar (sticky)
      popup = lapply(tooltip, HTML),
      labelOptions = labelOptions(
        direction = "auto",
        textsize  = "13px"
        
      ),
      highlightOptions = highlightOptions(
        weight      = 2,
        color       = "#2c3e50",
        fillOpacity = 0.95,
        bringToFront = TRUE
      ),
      options = pathOptions(className = paste0("uf_", a))
    )
}

# ===============================================================
# 8. CAMADA BRASIL (nível Brasil)
# ===============================================================

mapa <- mapa %>%
  addPolygons(
    data        = geo_brasil,
    fillColor   = "#43A047",
    fillOpacity = 0.75,
    color       = "white",
    weight      = 1,
    options     = pathOptions(className = "brasil_layer")
  )

# ===============================================================
# 9. CAMADAS MUNDO (FAO)
# ===============================================================

for (a in anos_fao) {
  
  shp <- mundo[[as.character(a)]]
  
  popup <- paste0(
    "<div style='font-family:Arial;font-size:13px;'>",
    "<strong>", shp$NAME, "</strong><br><br>",
    "Ano: ", a, "<br><br>",
    "Insegurança alimentar: <b>",
    ifelse(is.na(shp$Total), "Não informado", paste0(round(shp$Total, 1), "%")),
    "</b><br><br>",
    "Homens: <b>",
    ifelse(is.na(shp$Homens), "Não informado", paste0(round(shp$Homens, 1), "%")),
    "</b><br>",
    "Mulheres: <b>",
    ifelse(is.na(shp$Mulheres), "Não informado", paste0(round(shp$Mulheres, 1), "%")),
    "</b></div>"
  )
  
  mapa <- mapa %>%
    addPolygons(
      data         = shp,
      fillColor    = ~pal(Total),
      fillOpacity  = 0.8,
      color        = "white",
      weight       = 1,
      popup        = lapply(popup, HTML),
      # contorno ao clicar/passar no país
      highlightOptions = highlightOptions(
        weight      = 3,
        color       = "#2c3e50",
        fillOpacity = 0.95,
        bringToFront = TRUE
      ),
      options = pathOptions(className = paste0("mundo_", a))
    )
}

# ===============================================================
# 10. PAINEL
# ===============================================================

painel <- tags$div(
  style = 'background:white;padding:10px;border-radius:10px;
           box-shadow:0 2px 12px rgba(0,0,0,0.2);font-family:Arial;width:150px;',
  
  tags$div(style = 'font-size:11px;color:#888;margin-bottom:6px;', 'FONTE'),
  
  tags$div(
    style = 'display:flex;gap:6px;margin-bottom:12px;',
    tags$button('PNAD-IBGE', id = 'btn_ibge', onclick = "trocarFonte('ibge')",
                style = 'flex:1;background:#2980b9;color:white;border:none;padding:8px;border-radius:6px;cursor:pointer;'),
    tags$button('FAO', id = 'btn_fao', onclick = "trocarFonte('fao')",
                style = 'flex:1;background:white;border:1px solid #ddd;padding:8px;border-radius:6px;cursor:pointer;')
  ),
  
  tags$div(style = 'font-size:11px;color:#888;margin-bottom:6px;', 'ANO'),
  
  tags$div(id = 'painel_anos',
           style = 'display:flex;flex-direction:column;gap:5px;margin-bottom:12px;'),
  
  tags$div(id = 'titulo_nivel',
           style = 'font-size:11px;color:#888;margin-bottom:6px;', 'NÍVEL'),
  
  tags$div(
    id    = 'painel_nivel',
    style = 'display:flex;gap:6px;margin-bottom:12px;',
    tags$button('UFs', id = 'btn_ufs', onclick = "trocarNivel('ufs')",
                style = 'flex:1;background:#2980b9;color:white;border:none;padding:8px;border-radius:6px;cursor:pointer;'),
    tags$button('Brasil', id = 'btn_brasil', onclick = "trocarNivel('brasil')",
                style = 'flex:1;background:white;border:1px solid #ddd;padding:8px;border-radius:6px;cursor:pointer;')
  ),
  
  tags$div(
    id    = 'painel_grupos',
    style = 'display:none;',
    tags$div(style = 'font-size:11px;color:#888;margin-bottom:6px;', 'GRUPO'),
    tags$button('Total',    id = 'grupo_total',    onclick = "trocarGrupo('total')",
                style = 'width:100%;margin-bottom:3px;padding:5px;background:#2980b9;color:white;border:none;border-radius:6px;cursor:pointer;'),
    tags$button('Raça/cor', id = 'grupo_raca',     onclick = "trocarGrupo('raca')",
                style = 'width:100%;margin-bottom:3px;padding:5px;background:white;border:1px solid #ddd;border-radius:6px;cursor:pointer;'),
    tags$button('Sexo',     id = 'grupo_sexo',     onclick = "trocarGrupo('sexo')",
                style = 'width:100%;margin-bottom:3px;padding:5px;background:white;border:1px solid #ddd;border-radius:6px;cursor:pointer;'),
    tags$button('Educação', id = 'grupo_educacao', onclick = "trocarGrupo('educacao')",
                style = 'width:100%;margin-bottom:3px;padding:5px;background:white;border:1px solid #ddd;border-radius:6px;cursor:pointer;'),
    tags$button('Idade',    id = 'grupo_idade',    onclick = "trocarGrupo('idade')",
                style = 'width:100%;margin-bottom:3px;padding:5px;background:white;border:1px solid #ddd;border-radius:6px;cursor:pointer;'),
    tags$button('Profissão',  id = 'grupo_profissao',   onclick = "trocarGrupo('profissao')",
                style = 'width:100%;margin-bottom:3px;padding:5px;background:white;border:1px solid #ddd;border-radius:6px;cursor:pointer;'
    )
  )
)

# ===============================================================
# 11. INFO BOX (dados Brasil)
# ===============================================================

info_box <- tags$div(
  id    = 'info_box',
  style = 'position:absolute;  top:90px;  right:30px;  width:280px;  max-height:420px;  overflow-y:auto;
  background:white;  padding:14px;  border-radius:12px;  z-index:1000;  display:none;
  font-family:Arial;  font-size:12px;  line-height:1.3;  box-shadow:0 2px 10px rgba(0,0,0,0.18);

'
)

# ===============================================================
# 12. TÍTULO — centralizado via CSS position:fixed
# ===============================================================

titulo <- tags$div(
  'Segurança Alimentar — Brasil e Mundo',
  style = 'position:fixed;top:12px;left:50%;transform:translateX(-50%);
           z-index:1000;background:#3d8cc7;color:white;padding:10px 22px;
           border-radius:8px;font-family:Arial;font-size:15px;font-weight:bold;
           box-shadow:none;white-space:nowrap;'
)

# ===============================================================
# 13. LOGO
# ===============================================================

logo <- tags$div(
  
  tags$img(
    
    src = "https://i.imgur.com/71dyK85.png",
    
    style = "height:70px;"
    
  ),
  
  style = "

    background:transparent;

    padding:0px;

    box-shadow:none;

  "
  
)
# ===============================================================
# BOTAO INFO
# ===============================================================

info_button <- tags$div(
  
  HTML("ℹ"),
  
  style = '

    background:white;

    padding:10px 14px;

    border-radius:10px;

    box-shadow:none;

    font-family:Arial;

    cursor:pointer;

    font-weight:bold;

  ',
  
  `data-toggle` = "modal",
  
  `data-target` = "#infobox"
  
)

# ===============================================================
# INFOBOX HTML
# ===============================================================

info.box <- HTML(paste0(
  
  '
<div class="modal fade"
     id="infobox"
     role="dialog">

  <div class="modal-dialog modal-lg">

    <div class="modal-content">

      <div class="modal-header">

        <button type="button"
                class="close"
                data-dismiss="modal">

          &times;

        </button>

        <h2 style="
            margin-top:5px;
            font-family:Arial;
            font-weight:bold;
            color:#2c3e50;
        ">

          Segurança alimentar — Brasil e mundo

        </h2>

      </div>

      <div class="modal-body"
           style="
             font-family:Arial;
             line-height:1.7;
             font-size:15px;
           ">

        <p>

          Este estudo apresenta indicadores de
          segurança e insegurança alimentar
          para o Brasil, estados brasileiros
          e países do mundo.

        </p>

        <hr>

        <h3>Fontes</h3>

        <p>
          • PNAD Contínua — IBGE
        </p>

        <p>
          • Food Insecurity Experience Scale (FIES) — FAO
        </p>

        <hr>

        <h3>Indicadores IBGE</h3>

        <p>
          • Segurança alimentar
        </p>

        <p>
          • Insegurança alimentar
        </p>

        <p>
          • Insegurança alimentar leve
        </p>

        <p>
          • Insegurança alimentar moderada
        </p>

        <p>
          • Insegurança alimentar grave
        </p>

        <hr>

        <h3>Indicadores FAO</h3>

        <p>

          • Prevalência de insegurança alimentar
          moderada ou grave
          (Prevalence of moderate or severe food insecurity)

        </p>

        <p>

          Os valores apresentados pela FAO
          correspondem a médias móveis
          de três anos.

        </p>

        <hr>

        <h3>Agregações e metodologia</h3>

        <p>

          A pesquisa do IBGE é realizada por domicílio,
          considerando a situação do responsável pelo domicílio.
          A categoria de idade, entretanto, foi calculada por morador,
          considerando a idade individual das pessoas pesquisadas.

        </p>

        <p>

          Os indicadores dos grupos sociais
          foram calculados considerando o total
          de indivíduos dentro do próprio grupo.

        </p>

        <p>

          Por exemplo, a porcentagem de pessoas pardas
          em segurança alimentar corresponde ao total
          de pessoas pardas em segurança alimentar
          dividido pelo total de pessoas pardas.

        </p>

        <p>

          O mesmo procedimento foi realizado para
          sexo, idade e escolaridade, permitindo
          comparações proporcionais entre grupos sociais.

        </p>

        <h4>Educação</h4>

        <p>

          <b>Até o ensino fundamental completo</b>
          engloba:

        </p>

        <p style="margin-left:20px;">

          • Sem instrução<br>
          • Ensino fundamental incompleto<br>
          • Ensino fundamental completo

        </p>

        <p>

          <b>Até o ensino médio completo</b>
          engloba:

        </p>

        <p style="margin-left:20px;">

          • Ensino médio incompleto<br>
          • Ensino médio completo

        </p>

        <p>

          <b>Até o ensino superior completo</b>
          engloba:

        </p>

        <p style="margin-left:20px;">

          • Ensino superior incompleto<br>
          • Ensino superior completo

        </p>

        <hr>

        <h3>Elaboração</h3>

        <p>

          Aluna Laura Silveira Alves para a disciplina de
          <b>

          Sociobiodiversidade, soberania e segurança alimentar e nutricional

          </b>

          da professora
         

          Gabriela Coelho de Souza

          

        </p>

      </div>

    </div>

  </div>

</div>

'
  
))
# ===============================================================
# 15. CONTROLES
# ===============================================================

mapa <- mapa %>%
  
  addControl(painel,      position = 'topleft') %>%
  addControl(info_box,    position = 'topright') %>%
  addControl(titulo,      position = 'topleft') %>%   # CSS fix centraliza
  addControl(logo,        position = 'bottomright') %>%
  addControl(
    tags$div(
      style='
      background:transparent;
      box-shadow:none;
      border:none;
    ',
      info_button
    ),
    position = 'bottomleft'
  )

# ===============================================================
# 16. JAVASCRIPT — escrito em arquivo separado para evitar
#     conflitos de escape dentro do paste0() do R
# ===============================================================

# Escreve o JSON dos dados do Brasil numa variável que o JS vai usar
json_line <- paste0("var dadosBrasil = ", json_brasil, ";")

# O JS completo — sem paste0, sem escapes problemáticos
js_text <- paste(
  "function(el,x){",
  "  var map = this;",
  json_line,
  "  var fonteAtual = 'ibge';",
  "  var anoIBGE    = '2023';",
  "  var anoFAO     = '2023';",
  "  var nivelAtual = 'ufs';",
  "  var grupoAtual = 'total';",
  
  # ── esconder tudo ────────────────────────────────────────────
  "  function esconderTudo(){",
  "    $('.uf_2023,.uf_2024').hide();",
  "    $('.mundo_2016,.mundo_2017,.mundo_2018,.mundo_2019').hide();",
  "    $('.mundo_2020,.mundo_2021,.mundo_2022,.mundo_2023').hide();",
  "    $('.brasil_layer').hide();",
  "  }",
  
  # ── nomes legíveis dos grupos ────────────────────────────────
  
  "  function nomeGrupo(g){",
  
  "    var n = {",
  
  "      'Ate_17_anos': 'Até 17 anos',",
  
  "      '18_a_64_anos': 'De 18 a 64 anos',",
  
  "      '65_anos_ou_mais': '65 anos ou mais',",
  
  "      'Baixa_escolaridade': 'Até o Ensino Fundamental completo',",
  
  "      'Medio_completo': 'Até o Ensino Médio completo',",
  
  "      'Superior_completo': 'Até o Ensino Superior Completo',",
  
  "      'Trabalhador_Domestico': 'Trabalhador doméstico',",
  
  "      'Empregado_Privado_Carteira':",
  "      'Empregado privado com carteira assinada',",
  
  "      'Empregado_Privado_SEM_Carteira':",
  "      'Empregado privado sem carteira assinada',",
  
  "      'Empregado_Setor_Publico':",
  "      'Empregado no setor público',",
  
  "      'Conta_Propria': 'Conta própria',",
  
  "      'Empregador': 'Empregador',",
  
  "      'Outros': 'Outros'",
  
  "    };",
  
  "    return n[g] || g;",
  
  "  }",
  
  # ── gerar HTML dos cards Brasil ──────────────────────────────
  "  function gerarHTMLBrasil(){",
  "    var filtrado = dadosBrasil.filter(function(d){",
  "      return String(d.Ano) == anoIBGE && d.Categoria == grupoAtual;",
  "    });",
  "    var html = '';",
  "    filtrado.forEach(function(d){",
  "      var ng = nomeGrupo(d.Grupo);",
  "      html += '<div style=\"margin-bottom:14px;border:1px solid #e5e5e5;border-radius:9px;padding:10px;font-size:13px;\">'",
  "      html += '<div style=\"font-size:16px;font-weight:bold;margin-bottom:8px;line-height:1.25;\">' + ng + '</div>';",
  "      html += '<div style=\"margin-bottom:6px;\">Segurança alimentar: <b>' + d.Seg + '%</b></div>';",
  "      html += '<div style=\"margin-bottom:8px;\">Insegurança alimentar: <b>' + d.Ia + '%</b></div>';",
  "      html += '<div style=\"margin-left:6px;line-height:1.45;\">';",
  "      html += 'Leve: ' + d.Leve + '%<br>';",
  "      html += 'Moderada: ' + d.Mode + '%<br>';",
  "      html += 'Grave: ' + d.Grave + '%';",
  "      html += '</div></div>';",
  "    });",
  "    return html;",
  "  }",
  
  # ── mostrar info Brasil ──────────────────────────────────────
  "  function mostrarInfoBrasil(){",
  "    $('#info_box').show().html(gerarHTMLBrasil());",
  "  }",
  
  # ── atualizar mapa ───────────────────────────────────────────
  "  function atualizarMapa(){",
  "    esconderTudo();",
  "    $('#info_box').hide();",
  "    if(fonteAtual == 'ibge'){",
  "      if(nivelAtual == 'ufs'){ $('.uf_' + anoIBGE).show(); }",
  "      else { $('.brasil_layer').show(); }",
  "    } else {",
  "      $('.mundo_' + anoFAO).show();",
  "    }",
  "  }",
  
  "  atualizarMapa();",
  
  # ── click Brasil ─────────────────────────────────────────────
  "  map.eachLayer(function(layer){",
  "    if(layer.options.className == 'brasil_layer'){",
  "      layer.on('click', function(){",
  "        if(fonteAtual=='ibge' && nivelAtual=='brasil'){ mostrarInfoBrasil(); }",
  "      });",
  "    }",
  "  });",
  
  # ── botões ano IBGE ──────────────────────────────────────────
  "  function atualizarAnosIBGE(){",
  "    var b23 = anoIBGE=='2023' ? '#2980b9' : 'white';",
  "    var c23 = anoIBGE=='2023' ? 'white'   : '#333';",
  "    var b24 = anoIBGE=='2024' ? '#2980b9' : 'white';",
  "    var c24 = anoIBGE=='2024' ? 'white'   : '#333';",
  "    var html = '<div style=\"display:flex;gap:6px;\">';",
  "    html += '<button onclick=\"trocarAnoIBGE(2023)\" style=\"flex:1;padding:8px;border-radius:6px;border:none;background:' + b23 + ';color:' + c23 + ';cursor:pointer;\">2023</button>';",
  "    html += '<button onclick=\"trocarAnoIBGE(2024)\" style=\"flex:1;padding:8px;border-radius:6px;border:1px solid #ddd;background:' + b24 + ';color:' + c24 + ';cursor:pointer;\">2024</button>';",
  "    html += '</div>';",
  "    $('#painel_anos').html(html);",
  "  }",
  
  # ── botões ano FAO ───────────────────────────────────────────
  "  function atualizarAnosFAO(){",
  "    var anos = [2023,2022,2021,2020,2019,2018,2017,2016];",
  "    var html = '';",
  "    anos.forEach(function(a){",
  "      var ativo = String(a) == anoFAO;",
  "      var bg    = ativo ? '#2980b9' : 'white';",
  "      var cor   = ativo ? 'white'   : '#333';",
  "      var brd   = ativo ? 'none'    : '1px solid #ddd';",
  "      var mb    = a==2016 ? '' : 'margin-bottom:3px;';",
  "      html += '<button onclick=\"trocarAnoFAO(' + a + ')\"';",
  "      html += ' style=\"width:100%;' + mb + 'padding:8px;border-radius:6px;border:' + brd + ';background:' + bg + ';color:' + cor + ';cursor:pointer;font-size:13px;\">' + a + '</button>';",
  "    });",
  "    $('#painel_anos').html(html);",
  "  }",
  
  "  atualizarAnosIBGE();",
  
  # ── trocar fonte ─────────────────────────────────────────────
  "  window.trocarFonte = function(fonte){",
  "    fonteAtual = fonte;",
  "    if(fonte=='ibge'){",
  "      $('#btn_ibge').css({'background':'#2980b9','color':'white','border':'none'});",
  "      $('#btn_fao').css({'background':'white','color':'#333','border':'1px solid #ddd'});",
  "      $('#painel_nivel,#titulo_nivel').show();",
  "      atualizarAnosIBGE();",
  "      map.setView([-14,-52], 4);",
  "    } else {",
  "      $('#btn_fao').css({'background':'#2980b9','color':'white','border':'none'});",
  "      $('#btn_ibge').css({'background':'white','color':'#333','border':'1px solid #ddd'});",
  "      $('#painel_nivel,#titulo_nivel,#painel_grupos').hide();",
  "      $('#info_box').hide();",
  "      atualizarAnosFAO();",
  "      map.setView([15,-15], 3);",
  "    }",
  "    atualizarMapa();",
  "  };",
  
  # ── trocar nível ─────────────────────────────────────────────
  "  window.trocarNivel = function(nivel){",
  "    nivelAtual = nivel;",
  "    if(nivel=='ufs'){",
  "      $('#btn_ufs').css({'background':'#2980b9','color':'white','border':'none'});",
  "      $('#btn_brasil').css({'background':'white','color':'#333','border':'1px solid #ddd'});",
  "      $('#painel_grupos,#info_box').hide();",
  "    } else {",
  "      $('#btn_brasil').css({'background':'#2980b9','color':'white','border':'none'});",
  "      $('#btn_ufs').css({'background':'white','color':'#333','border':'1px solid #ddd'});",
  "      $('#painel_grupos').show();",
  "    }",
  "    atualizarMapa();",
  "  };",
  
  # ── trocar ano IBGE ──────────────────────────────────────────
  "  window.trocarAnoIBGE = function(ano){",
  "    anoIBGE = String(ano);",
  "    atualizarAnosIBGE();",
  "    atualizarMapa();",
  "  };",
  
  # ── trocar ano FAO ───────────────────────────────────────────
  "  window.trocarAnoFAO = function(ano){",
  "    anoFAO = String(ano);",
  "    atualizarAnosFAO();",
  "    atualizarMapa();",
  "  };",
  
  # ── trocar grupo Brasil ──────────────────────────────────────
  "  window.trocarGrupo = function(grupo){",
  "    grupoAtual = grupo;",
  "    var ids = ['total','raca','sexo','educacao','idade','profissao'];",
  "    ids.forEach(function(g){",
  "      $('#grupo_' + g).css({'background':'white','color':'#333','border':'1px solid #ddd'});",
  "    });",
  "    $('#grupo_' + grupo).css({'background':'#2980b9','color':'white','border':'none'});",
  "    mostrarInfoBrasil();",
  "  };",
  
  "}",
  sep = "\n"
)

# ===============================================================
# 17. APLICAR JS
# ===============================================================

mapa <- mapa %>% onRender(js_text) %>%
prependContent(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(
      src = "https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js"
    ),
    tags$script(
      src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js"
    )
  )
) %>%
  
  htmlwidgets::appendContent(
    info.box
  )

mapa


# ===============================================================
# 18. SALVAR
# ===============================================================
# selfcontained = FALSE evita erro de memória (pandoc)
# Gera: index.html + pasta index_files/
# Para GitHub Pages: suba AMBOS no repositório
options(htmlwidgets.TOJSON_ARGS = list(auto_unbox = TRUE))
saveWidget(
  widget = mapa,
  file = "index.html",
  selfcontained = TRUE,
  title = "Segurança Alimentar — Brasil e Mundo"
)
message("✔ index.html gerado!")
message("  Abra no Chrome: index.html")
message("  GitHub Pages: suba index.html E a pasta index_files/")