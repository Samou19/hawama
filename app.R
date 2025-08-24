library(shiny)
library(DT)
library(janitor)   # 📌 pour adorn_totals
library(openxlsx)
library(googlesheets4)
library(gmailr)
library(janitor)
library("gargle")
library(googledrive)
library(usethis)


# Authentification avec le compte de service
#gs4_auth(path = ".secrets/service-account.json")
#sheet_id <- "1ryfmQZk0HJt5oLC3yBF5ctu-Dwjrt09-SPuhDrSGurk"

# Spécifie le chemin vers ta clé JSON
service_account_key <- ".secrets/service-account.json"

# Authentification via compte de service
gs4_auth(path = service_account_key)
drive_auth(path = service_account_key)

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
sheet_id <- googledrive::drive_get("Articles Hawama")$id

ui <- fluidPage(
  titlePanel("Calculateur Hawama"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        background-color: #f4f4f4;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .btn {
        background-color: #007bff;
        color: white;
      }
      .btn:hover {
        background-color: #0056b3;
      }
      
       hr {
        border: 1px solid #ccc;
        margin: 15px 0;
        }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("nom", "Nom de l'article :", "", placeholder = "Entrez le nom de l'article"),
      numericInput("prix_shein", "Prix Shein (€) :", value = 0, min = 0, step = 0.01),
      numericInput("taux_cfa", "Taux CFA :", value = 655, min = 1, step = 1),
      numericInput("transport", "Transport (en %) :", value = 0.35, min = 0, step = 0.01),
      numericInput("marge", "Marge (en %) :", value = 0.30, min = 0, step = 0.01),
      actionButton("ajouter", "➕ Ajouter un article"),
      br(), br(),
      actionButton("modifier", "✏️ Modifier la ligne sélectionnée"),
      br(), br(),
      actionButton("supprimer", "🗑️ Supprimer la ligne sélectionnée"),
      hr(),  # <-- ligne horizontale
      actionButton("charger_gs", "📥 Charger depuis Google Sheets"),
      br(), br(),
      actionButton("sauvegarde_gs", "☁️ Sauvegarder sur Google Sheets")),
    
    mainPanel(
      h3("Tableau des articles"),
      DTOutput("tableau")
    )
  )
)

# https://docs.google.com/spreadsheets/d/1DffFGdokwyUIuZbqznIlANb8zvhv1bMSTTbGXdEvYrA/edit?gid=0#gid=0

server <- function(input, output, session) {
  
  # Sauvegarder sur Google Sheets
# Sauvegarder les données dans la Google Sheet
  observeEvent(input$sauvegarde_gs, {
    req(nrow(articles()) > 0) # vérifier qu'il y a des données
    sheet_write(data = articles(),
                ss = sheet_id,
                sheet = "main")
  })
  # Charger les données depuis la Google Sheet
  observeEvent(input$charger_gs, {
    # Read our sheet
    data <- read_sheet(ss = sheet_id, 
                         sheet = "main")
    #data <- googlesheets4::read_sheet(ss, sheet = "Articles")
    articles(data)
  })
  # fin
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
  
  # Charger les valeurs d'une ligne sélectionnée dans les inputs
  observeEvent(input$tableau_rows_selected, {
    sel <- input$tableau_rows_selected
    if (length(sel)) {
      row <- articles()[sel, ]
      updateTextInput(session, "nom", value = row$Nom)
      updateNumericInput(session, "prix_shein", value = row$`Prix Shein (€)`)
      updateNumericInput(session, "taux_cfa", value = row$`Taux CFA`)
      updateNumericInput(session, "transport", value = row$`Transport (%)`)
      updateNumericInput(session, "marge", value = row$`Marge (%)`)
    }
  })
  
  # Modifier la ligne sélectionnée
  observeEvent(input$modifier, {
    sel <- input$tableau_rows_selected
    req(length(sel) == 1)  # assure qu'une ligne est bien sélectionnée
    
    prix_achat <- (1 + input$transport) * input$prix_shein * input$taux_cfa
    prix_vente <- (1 + input$marge) * prix_achat
    benefice <- prix_vente - prix_achat
    
    data <- articles()
    
    data[sel, ] <- list(
      input$nom,
      input$prix_shein,
      input$taux_cfa,
      input$transport,
      round(prix_achat, 0),
      input$marge,
      round(prix_vente, 0),
      round(benefice, 0)
    )
    articles(data)
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
    
    data <- articles()
    
    if (nrow(articles()) > 1) {
      # 👉 ajoute une ligne TOTAL sur les colonnes numériques
      # data <- adorn_totals(data, where = "row", fill = "-", na.rm = TRUE,,,,
      #                      "Prix Shein (€)", "Prix de vente (CFA)",
      #                      "Bénéfice (CFA)")
      data <- articles()
      # print(colnames(data))
      data <- adorn_totals(data, where = "row",,,,"Prix.Shein....", "Prix.d.achat..CFA.",
                           "Prix.de.vente..CFA.", "Bénéfice..CFA.")
      
    }
    #data <- articles()
    
    datatable(
      data,
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
        columnDefs = list(list(className = "dt-center", targets = "_all")), # centre toutes les colonnes
        searching = FALSE
      ),
      extensions = 'Buttons'
    )
  })
}

# export du sheet
# aa=googledrive::drive_get("Articles Hawama")
# aa$drive_resource[[1]]
# $exportLinks$`application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`
# [1] "https://docs.google.com/spreadsheets/export?id=1ryfmQZk0HJt5oLC3yBF5ctu-Dwjrt09-SPuhDrSGurk&exportFormat=xlsx"

shinyApp(ui, server)
