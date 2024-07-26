dashboardPage(
  dashboardHeader(title = "",
                  titleWidth = 0),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Aplicação", tabName = "A", icon = icon("gear")),
      menuItem("Sobre", tabName = "B", icon = icon("info-circle")),
      menuItem("Contato", tabName = "C", icon = icon("address-book"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$title("Título da Aba do Navegador")),
    
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Similaridade de veículos da Tabela Fipe </span>\');
      })
     ')),
    
    tabItems(
      tabItem(
        tabName = "A",
        
        fluidRow(
          box(width = "5", 
              column(width = 11, htmlOutput("idTexto"),
                     selectInput(inputId = "idAno", label = "Ano:",
                                 choices = sort(unique(Tabela_clust$anoModelo), 
                                                decreasing = TRUE)
                                 ),
                     
                     selectInput(inputId = "idMarca", label = "Marca:",
                                 choices = sort(unique(Tabela_clust$Marca))
                                 ),
                     
                     selectInput(inputId = "idModelo", label = "Modelo:",
                                 choices = sort(Tabela_clust.plot$Modelo)
                                 ),
                     
                     actionButton("idConsultar", "Consultar", class = "btn-success")
              )),
          
          box(width = "6",
              column(width = 12,
                     tableOutput("idTabela"),
                     htmlOutput("TextoCarro"),
                     htmlOutput("mensagemInicial")
              ))
          
        )

      ),
      
      tabItem(
        tabName = "B",
        h1("Sobre:"),
        h4("Esta é uma aplicação estatística envolvendo clusterização utilizando 
        dados de veículos da", a("Tabela Fipe",
                                 href = "https://veiculos.fipe.org.br/",
                                 target = "_blank"), 
        "do mês de junho de 2024 usando a linguagem R."),
        h4("Trata-se sobre uma ferramenta criada para o usuário selecionar um veículo
           e obter como resposta, os 10 veículos mais similares da respectiva escolha.
           Para isso, foi utilizado uma técnica de mineração de dados chamada", 
           a("k-means", href = "https://pt.wikipedia.org/wiki/K-means", target = "_blank"),
           "e a métrica de distância entre os pontos chamada de", 
           a("Distância Euclidiana", href = "https://pt.wikipedia.org/wiki/Dist%C3%A2ncia_euclidiana",
             target = "_blank"), "."),
        h4("Para a clusterização, utilizamos as variáveis:"),
        h4(HTML(
          paste("<ul>",
                "<li>Ano</li>",
                "<li>Câmbio</li>",
                "<li>Cilindros</li>",
                "<li>Cilindradas</li>",
                "<li>Combustível</li>",
                "<li>Tração</li>",
                "<li>Válvulas</li>",
                "<li>Preço</li>",
                "</ul>")
           )
        ),
        h4("Dentro do nosso conjunto de dados, não estão incluso motos, 
           caminhões e veículos elétricos. Além de que selecionamos 
           apenas veículos fabricados a partir do ano 2000.")
      
      ),
      
      tabItem(
        tabName = "C",
        h1("Contatos:"),
        h3(a("Makson Pedro Rodrigues", 
             href = "https://lacid.ccet.ufrn.br/author/makson-pedro-rodrigues/",
             target = "_blank")),
        h4("\u2709 maksonpedro@gmail.com"),
        h3(a("Marcelo Bourguignon Pereira", 
             href = "https://lacid.ccet.ufrn.br/author/marcelo-bourguignon/",
             target = "_blank")),
        h4("\u2709 marcelo.bourguignon@ufrn.br"),
        h3(a("Marcus A. Nunes", 
             href = "https://lacid.ccet.ufrn.br/author/marcus-a.-nunes/",
             target = "_blank")),
        h4("\u2709 marcus.nunes@ufrn.br")
      )
    
    )
    
  ),
  
  skin = "purple"
)
