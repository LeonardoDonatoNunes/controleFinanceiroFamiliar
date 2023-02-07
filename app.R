library(shiny)
library(googlesheets4)
library(reactable)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(tidyr)

options(shiny.launch.browser = TRUE, shiny.autoreload = TRUE)

# Define UI for application that draws a histogram

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Controle Financeiro"),
                    dashboardSidebar(
                      uiOutput('seletores_ui')
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")    ),
                      shinyjs::useShinyjs(),
                      uiOutput('visualizacoes_ui')
                    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dados <- reactiveValues()
  
  output$seletores_ui <- renderUI({
    
    dados$pagamentos <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1pD0WOSpT5kCzS_-17nsz0zFU9iHNeTQKs27H6Tg_a9w/edit?usp=sharing", sheet = 'pagamentos')
    dados$usuario <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1pD0WOSpT5kCzS_-17nsz0zFU9iHNeTQKs27H6Tg_a9w/edit?usp=sharing", sheet = 'usuario')
    dados$categoria_gasto <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1pD0WOSpT5kCzS_-17nsz0zFU9iHNeTQKs27H6Tg_a9w/edit?usp=sharing", sheet = 'categoria_gasto')
    
    #dados$usuario <- dados$usuario %>% mutate(id = as.numeric(id))
    
    fluidPage(
      uiOutput('seletores_sidebar_ui')
    )
    
  })
  
  output$seletores_sidebar_ui <- renderUI({
    
    dados_pagamentos <- !is.null(dados$pagamentos)
    dados_categorias <- !is.null(dados$categoria_gasto)
    dados_usuarios <- !is.null(dados$usuario)
    
    if (all(dados_pagamentos, dados_usuarios, dados_categorias)) {
      
      dados$categoria_gasto <- isolate(dados$categoria_gasto %>% arrange(nome))
      
      vct_usuario <- dados$usuario$id %>% setNames(dados$usuario$nome)
      vct_categoria_gastos <- dados$categoria_gasto$id %>% setNames(dados$categoria_gasto$nome)
      
      
      tagList(
        selectInput('usuario_sel', "Usuário", choices = vct_usuario),
        actionLink('cadastrar_receita', "Cadastrar receita", icon = icon("hand-holding-dollar")),
        selectInput('categoria_gasto_sel', "Categoria", choices = vct_categoria_gastos),
        actionLink('nova_categoria_gasto', "Nova categoria de gasto", icon = icon("square-plus")),
        currencyInput('valor', "Valor", value = 0, format = "Brazilian"),
        dateInput('data', label = 'Data'),
        actionButton('salvar_gasto', "Salvar", icon = icon("cloud-arrow-up"))
      )
      
    }
    
  })
  
  observeEvent(input$cadastrar_receita, {
    showModal(modalDialog(
      title = NULL,
      easyClose = FALSE,
      footer = list(modalButton('Calcelar'), actionButton('salvar_nova_receita', "Salvar")),
      size = 's',
      fluidPage(
        h3("Nova receita"),
        textInput('nome_nova_receita', NULL, placeholder = "Nome da receita"),
        currencyInput('valor_nova_receita', NULL, value = 0, format = "Brazilian"),
        checkboxInput('receita_mensal', "Receita mensal", value = TRUE),
        actionLink('gerenciar_receitas', "Ver dados completos", icon = icon("table"))
      )
    ))
  })
  
  
  observeEvent(input$nova_categoria_gasto, {
    showModal(modalDialog(
      title = NULL,
      easyClose = FALSE,
      footer = list(modalButton('Calcelar'), actionButton('salvar_nova_categoria', "Salvar")),
      size = 's',
      fluidPage(
        h3("Nova categoria de gasto"),
        textInput('nome_nova_categoria', NULL, placeholder = "Nome da categoria"),
        checkboxInput('gasto_fixo', "Gasto fixo", value = TRUE),
        checkboxInput('gasto_compartilhado', "Gasto compartilhado", value = TRUE),
        checkboxInput('gasto_exclusivo_usuario', "Gasto exclusivo do usuário", value = TRUE),
        actionLink('gerenciar_categorias', "Ver dados completos", icon = icon("table"))
        
      )
    ))
  })
  
  
  observeEvent(input$salvar_nova_categoria, {
    
    dados$categoria_gasto <- 
      insert_nova_categoria(
        nome = input$nome_nova_categoria,
        gasto_fixo = input$gasto_fixo, 
        compartilhado = input$gasto_compartilhado, 
        gasto_exclusivo_usuario = input$gasto_exclusivo_usuario, 
        usuario = input$usuario_sel
      )
    
    removeModal()
    
    shinyalert::shinyalert(title = "Nova categoria inserida", type = 'success')
    
  })
  
  observeEvent(input$gerenciar_categorias, {
    
    removeModal()
    
    showModal(modalDialog(
      title = NULL,
      easyClose = FALSE,
      footer = list(
        modalButton('Fechar'), 
        disabled(actionButton('deletar_categoria', "Deletar")),
        disabled(actionButton('editar_categoria', "Editar"))
      ),
      size = 'l',
      tagList(
        reactableOutput('tabela_categorias')
      )
    ))
    
  })
  
  output$tabela_categorias <- renderReactable({
    
    dados$categoria_gasto %>% 
      mutate(dplyr::across(.cols = c(gasto_fixo, compartilhado , gasto_exclusivo_usuario), ~if_else(.x, "\U0002713", "\U0002717"))) %>% 
      dplyr::left_join(dados$usuario %>% select(usuario_id = id, nome_usuario = nome)) %>% 
      reactable(
        onClick = 'select',
        selection = "single",
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d"),
          searchInputStyle = list(`align-self` = 'flex-start')
        ),
        searchable = TRUE,
        pagination = F,
        highlight = TRUE, 
        height = 300,
        outlined  =  TRUE,
        rowStyle = list(cursor = "pointer"),
        wrap = FALSE,
        columns = list(
          .selection = colDef(show = FALSE),
          id = colDef(show = FALSE),
          nome = colDef(name = "Nome"),
          gasto_fixo = colDef(name = "Gasto fixo", align = 'center'),
          compartilhado = colDef(name = "Compartilhado", align = 'center'),
          gasto_exclusivo_usuario = colDef(name = "Usuário", align = 'center'),
          nome_usuario = colDef(name = "Nome do usuário"),
          usuario_id = colDef(show = FALSE)
        )
      )
  })
  
  observeEvent(input$tabela_categorias__reactable__selected, ignoreNULL = FALSE, {
    
    if (!is.null(input$tabela_categorias__reactable__selected)){
      
      enable('deletar_categoria')
      enable('editar_categoria')
      
    } else {
      
      disable('deletar_categoria')
      disable('editar_categoria')
      
    }
    
  })
  
  
  observeEvent(input$deletar_categoria, {
    
    aux <-
      dados$categoria_gasto %>% 
      slice(-input$tabela_categorias__reactable__selected)
    
    url <- "https://docs.google.com/spreadsheets/d/1pD0WOSpT5kCzS_-17nsz0zFU9iHNeTQKs27H6Tg_a9w/edit?usp=sharing"
    sheet <- 'categoria_gasto'
    
    googlesheets4::write_sheet(aux, ss = url, sheet = sheet)
    
    dados$categoria_gasto <- aux
    
  })
  
  
  # Output | visualizacoes_ui ---------------------------------------------
  output$visualizacoes_ui <- renderUI({
    
    tagList(
      fluidRow(
        valueBoxOutput('renda_total'),
        valueBoxOutput('gasto_total')
      ),
      column(
        width = 4,
        reactable::reactableOutput('tabela_gastos')
      ),
      column(8)
    )
    
  })
  
  
  # Render | tabela_gastos -------------------------------------------------
  output$tabela_gastos <- renderReactable({
    
    if (!is.null(dados$pagamentos)) {
      
      reais = colFormat(currency = "BRL", separators = TRUE, locales = "pt-BR")
      
      dados$usuario %>% 
        select(
          usuario_id = id, 
          nome_usuario = nome) %>% 
        left_join(
          dados$pagamentos,
          by = "usuario_id"
        ) %>% 
        left_join(
          dados$categoria_gasto %>% 
            select(
              categoria_gasto_id = id, 
              nome_categoria = nome, 
              usuario_id, 
              usuario_categoria_id = usuario_id,
              compartilhado),
          by = "categoria_gasto_id"
        ) %>% 
        group_by(nome_usuario) %>% 
        summarise(
          "Gasto total" = sum(valor), 
          "Gasto compartilhado" = sum(valor[compartilhado], na.rm = TRUE),
          "Gasto particular" = sum(valor[!compartilhado], na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        tidyr::pivot_longer(cols = -nome_usuario) %>% 
        mutate(value = if_else(is.na(value), 0, value)) %>% 
        tidyr::pivot_wider(names_from = nome_usuario, values_from = value) %>%
        reactable::reactable(
          pagination = F,
          highlight = TRUE,
          outlined  =  TRUE,
          rowStyle = list(cursor = "pointer"),
          defaultColDef = colDef(
            align = "center",
            headerStyle = list(background = "#f7f7f8")
          ),
          wrap = FALSE,
          columns = list(
            name = colDef(name = "Resumo dos gastos", minWidth = 150, maxWidth = 500),
            Leonardo = colDef(format = reais),
            Taise = colDef(format = reais)
          )
        )
      
    }
    
  })
  
  # Render | caixas de valor -------------------------------------------------
  output$renda_total <- renderValueBox({
    valueBox(value = reais(10000), subtitle = "Renda total", icon = icon("sack-dollar"))
  })
  
  output$gasto_total <- renderValueBox({
    
    if (!is.null(dados$pagamentos)) {
      
      valor <- dados$pagamentos$valor %>% sum(na.rm = TRUE)
      valueBox(value = reais(valor), subtitle = "Gasto total", icon = icon("money-bill-transfer"))
      
    }
    
  })
  
  
  
  # Funções -----------------------------------------------------------------
  insert_nova_categoria <- function(nome, gasto_fixo, compartilhado, gasto_exclusivo_usuario, usuario) {
    
    url <- "https://docs.google.com/spreadsheets/d/1pD0WOSpT5kCzS_-17nsz0zFU9iHNeTQKs27H6Tg_a9w/edit?usp=sharing"
    sheet <- 'categoria_gasto'
    
    aux <- googlesheets4::read_sheet(
      ss = url,
      sheet = sheet)
    
    id <- (aux$id %>% max()) + 1
    
    dados <- 
      data.frame(
        id = id,
        nome = nome, 
        gasto_fixo = gasto_fixo, 
        compartilhado = compartilhado, 
        gasto_exclusivo_usuario = gasto_exclusivo_usuario, 
        usuario_id = as.numeric(usuario))
    
    googlesheets4::sheet_append(data = dados, ss = url, sheet = sheet)
    
    aux <- aux %>% dplyr::add_row(dados)
    
    return(aux)
    
  }
  
  reais <- scales::number_format(accuracy = 0.01,prefix = 'R$', big.mark = ".", decimal.mark = ",")
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
