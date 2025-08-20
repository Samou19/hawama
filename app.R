library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Calculateur Hawama"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("nom", "Nom de l'article :", ""),
      numericInput("prix_shein", "Prix Shein (€) :", value = 0, min = 0, step = 0.01),
      numericInput("taux_cfa", "Taux CFA :", value = 600, min = 1, step = 1),
      numericInput("transport", "Transport (en %) :", value = 0.1, min = 0, step = 0.01),
      numericInput("marge", "Marge (en %) :", value = 0.2, min = 0, step = 0.01),
      actionButton("ajouter", "➕ Ajouter l'article"),
      br(), br(),
      actionButton("supprimer", "🗑️ Supprimer la ligne sélectionnée")
    ),
    
    mainPanel(
      h3("Tableau des articles"),
      DTOutput("tableau")
    )
  )
)

server <- function(input, output, session) {
  # Stockage réactif des articles
  articles <- reactiveVal(data.frame(
    Nom = character(),
    `Prix Shein (€)` = numeric(),
    `Taux CFA` = numeric(),
    `Transport (%)` = numeric(),
    `Prix d'achat (CFA)` = numeric(),
    `Marge (%)` = numeric(),
    `Prix de vente (CFA)` = numeric(),
    `Bénéfice (CFA)` = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Ajouter un article
  observeEvent(input$ajouter, {
    prix_achat <- (1 + input$transport) * input$prix_shein * input$taux_cfa
    prix_vente <- (1 + input$marge) * prix_achat
    benefice <- prix_vente - prix_achat
    
    new_row <- data.frame(
      Nom = input$nom,
      `Prix Shein (€)` = input$prix_shein,
      `Taux CFA` = input$taux_cfa,
      `Transport (%)` = input$transport,
      `Prix d'achat (CFA)` = round(prix_achat, 0),
      `Marge (%)` = input$marge,
      `Prix de vente (CFA)` = round(prix_vente, 0),
      `Bénéfice (CFA)` = round(benefice, 0),
      stringsAsFactors = FALSE
    )
    
    articles(rbind(articles(), new_row))
  })
  
  # Supprimer une ligne sélectionnée
  observeEvent(input$supprimer, {
    req(input$tableau_rows_selected)  # nécessite une sélection
    data <- articles()
    data <- data[-input$tableau_rows_selected, ]  # suppression
    articles(data)
  })
  
  # Affichage du tableau interactif
  output$tableau <- renderDT({
    datatable(
      articles(),
      rownames = FALSE,  # pas de numéros de ligne
      selection = "single",  # sélection d’une seule ligne à la fois
      colnames = c(
        "Nom",
        "Prix Shein (€)",
        "Taux CFA",
        "Transport (%)",
        "Prix d'achat (CFA)",
        "Marge (%)",
        "Prix de vente (CFA)",
        "Bénéfice (CFA)"
      ),
      options = list(
        pageLength = 10,
        autoWidth = F,
        dom = 'Bfrtip',
        buttons = c('excel', 'pdf'),
        columnDefs = list(list(className = "dt-center", targets = "_all")) # centre toutes les colonnes
      ),
      extensions = 'Buttons'
    )
  })
}

shinyApp(ui, server)
