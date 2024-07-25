library(shiny)

function(input, output, session) {
  
  output$idTexto <- renderUI({
    HTML("<b>Selecione um veículo para identificar 
         quais são os 10 veículos mais similares:<b>")
  })
  
  # Deixar o filtro Marca de forma dinâmica conforme o ano
  observe({
    Ano_selected <- input$idAno
    
    # Filtrar subcategorias com base na categoria selecionada
    subcategories1 <- Tabela_clust[Tabela_clust$anoModelo == input$idAno, 
                                   "Marca"]
    
    # Atualizar as opções do input de subcategoria
    updateSelectInput(session, "idMarca", choices = subcategories1)
  })  
  
  # Deixar o filtro Modelo de forma dinâmica conforme a marca (e o ano)
  observe({
      
    Marca_selected <- input$idMarca
    Ano_selected <- input$idAno
    
    # Filtrar subcategorias com base na categoria selecionada
    subcategories2 <- Tabela_clust[Tabela_clust$Marca == Marca_selected & 
                                   Tabela_clust$anoModelo == input$idAno, 
                                   "Modelo"]
    
    # Atualizar as opções do input de subcategoria
    updateSelectInput(session, "idModelo", choices = subcategories2)
  })
  createHyperlinks <- function(carro) {
    paste0('<a href="https://www.olx.com.br/autos-e-pecas/carros-vans-e-utilitarios?q=', 
           gsub(" ", "%20", carro), '" target="_blank">', carro, '</a>')
  }
  
  dados_select <- eventReactive(input$idConsultar, {
    x <- dados_proximos[-1, input$idModelo]
    x <- as.data.frame(x)
    colnames(x) <- "Modelo"
    x <- left_join(x, Tabela_clust[, c(1, 2, 4)], by = "Modelo")
    x <- mutate(x, Posicao = seq(1:nrow(x)))
    x <- select(x, Posicao, Modelo, Marca, Valor)
    colnames(x) <- c("Ranking", "Modelo", "Marca", "Preço")
    x$Modelo <- createHyperlinks(x$Modelo)
    
    x
  }, ignoreNULL = FALSE)
  
  text_select <- eventReactive(input$idConsultar, {
    HTML("<b>Carro selecionado:</b> ", createHyperlinks(input$idModelo), "<br>", 
         "<b>Preço:</b> ", 
         Tabela_clust[Tabela_clust$Modelo == input$idModelo, 4])
  }, ignoreNULL = FALSE)
  
  output$mensagemInicial <- renderUI({
    if(input$idConsultar == 0){
      HTML("Por favor, selecione algum veículo e clique em
           consultar.")
    }else{
      NULL
    }
    
    
  })
  
  output$idTabela <- renderTable({
    if(input$idConsultar > 0){
      dados_select()
    }
    
    }, sanitize.text.function = function(x) x)
    
  
  output$TextoCarro <- renderUI({
    if(input$idConsultar > 0){
      text_select()
    }
    
  })
  

}
