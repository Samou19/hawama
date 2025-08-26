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
library(tidyverse)


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
        background-color: white;/*#f4f4f4*/
      }
      
        .well {
        background-color: #64b5f6; /* Gris clair */
        border-radius: 10px; /* Coins arrondis */
        padding: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
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
        
      /* Style général des boutons DataTables */
      .dt-button {
        background-color: #007bff !important; /* Couleur par défaut */
        color: white !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 6px 12px !important;
        font-weight: bold;
        margin-right: 5px;
      }

      /* Bouton Excel : vert */
      .buttons-excel {
        background-color: #28a745 !important;
      }

      /* Bouton PDF : rouge */
      .buttons-pdf {
        background-color: #dc3545 !important;
      }

      .dt-button:hover {
        opacity: 0.85;
      }
      /*Notification de gargement et sauvegarde
      .shiny-notification {
      position: fixed;
      top: 20px;
      right: 20px;
      background-color: #28a745;
      color: white;
      padding: 15px;
      border-radius: 8px;
      font-size: 16px;
      box-shadow: 0px 4px 8px rgba(0,0,0,0.2);
      }*/
  .shiny-notification {
    position: fixed;
    top: 20px;
    right: 20px;
    background-color: #28a745;
    color: white;
    padding: 12px 18px;
    border-radius: 10px;
    font-size: 15px;
    font-weight: bold;
    box-shadow: 0px 4px 12px rgba(0,0,0,0.25);
    opacity: 0;
    animation: fadeIn 0.6s forwards, fadeOut 0.6s 3.5s forwards;
  }

  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(-10px); }
    to { opacity: 1; transform: translateY(0); }
  }

  @keyframes fadeOut {
    from { opacity: 1; transform: translateY(0); }
    to { opacity: 0; transform: translateY(-10px); }
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
      hr(),
      actionButton("charger_gs", "📥 Charger depuis Google Sheets"),
      hr(),
      actionButton("ajouter", "➕ Ajouter un article"),
      br(), br(),
      actionButton("modifier", "✏️ Modifier la ligne sélectionnée"),
      br(), br(),
      actionButton("supprimer", "🗑️ Supprimer la ligne sélectionnée"),
      hr(),  # <-- ligne horizontale
      actionButton("sauvegarde_gs", "☁️ Sauvegarder sur Google Sheets")),
    
    mainPanel(
      h3("Tableau des articles"),
      DTOutput("tableau")
    )
  )
)

# https://docs.google.com/spreadsheets/d/1DffFGdokwyUIuZbqznIlANb8zvhv1bMSTTbGXdEvYrA/edit?gid=0#gid=0

server <- function(input, output, session) {
  
  # Stockage réactif des articles
  articles <- reactiveVal(data.frame(
    Nom = character(),
    Date = character(),
    `Prix Shein (€)` = numeric(),
    `Taux CFA` = numeric(),
    `Transport (%)` = numeric(),
    `Prix d'achat (CFA)` = numeric(),
    `Marge (%)` = numeric(),
    `Prix de vente (CFA)` = numeric(),
    `Bénéfice (CFA)` = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Sauvegarder sur Google Sheets
  # Charger les données depuis la Google Sheet
  vals <- reactiveValues(data = NULL)
  observeEvent(input$charger_gs, {
    # Read our sheet
    vals$data1 <- read_sheet(ss = sheet_id, sheet = "main")
    data <- read_sheet(ss = sheet_id, 
                       sheet = "main")
    #data <- googlesheets4::read_sheet(ss, sheet = "Articles")
    articles(data)
    
    # Après la sauvegarde réussie :
    showNotification(
      ui = "✅ Données chargées avec succès !",
      type = "message",  # types: "message", "warning", "error"
      duration = 10       # durée en secondes
    )
  })
  # Sauvegarder les données dans la Google Sheet
  observeEvent(input$sauvegarde_gs, {
    req(nrow(articles()) > 0) # vérifier qu'il y a des données
    # Quand l'utilisateur ne charge pas les données
    # data1 <- read_sheet(ss = sheet_id, 
    #                    sheet = "main")
    if (is.null(vals$data1) ) {
      # sheet_delete(ss = sheet_id, sheet = "main")
      # sheet_add(ss = sheet_id, sheet = "main")
      data <- read_sheet(ss = sheet_id, 
                         sheet = "main")
      data2 <- rbind(data, articles())
      sheet_write(data = data2,
                  ss = sheet_id,
                  sheet = "main")
    } else {
      # Vider la feuille avant d'écrire les nouvelles données
      # sheet_clear(ss = sheet_id, sheet = "main")
      sheet_write(data = articles(),
                  ss = sheet_id,
                  sheet = "main")
    }
    
    
    # Après la sauvegarde réussie :
    showNotification(
      ui = "✅ Données sauvegardées avec succès !",
      type = "message",  # types: "message", "warning", "error"
      duration = 10       # durée en secondes
    )
  })

  # fin

  
  # Ajouter un article
  observeEvent(input$ajouter, {
    prix_achat <- round((1 + input$transport) * input$prix_shein * input$taux_cfa / 100) * 100 
    prix_vente <- round((1 + input$marge) * prix_achat / 100) * 100 
    benefice <- prix_vente - prix_achat
    
    new_row <- data.frame(
      Nom = input$nom,
      Date = format(as.POSIXct(Sys.time(), tz = "Europe/Paris"), "%d-%m-%Y %H:%M:%S"),
      `Prix Shein (€)` = input$prix_shein,
      `Taux CFA` = input$taux_cfa,
      `Transport (%)` = input$transport,
      `Prix d'achat (CFA)` = round(prix_achat / 100) * 100,
      `Marge (%)` = input$marge,
      `Prix de vente (CFA)` = round(prix_vente / 100) * 100,
      `Bénéfice (CFA)` =  benefice,
      stringsAsFactors = FALSE
    )
    
    # Renommer les colonnes pour correspondre à celles de la Google Sheet
    new_row = new_row |> 
      rename_with(~ case_when(
      .x == "Prix.Shein...." ~ "Prix Shein (€)",
      .x == "Taux.CFA" ~ "Taux CFA",
      .x == "Transport...." ~ "Transport (%)",
      .x == "Prix.d.achat..CFA." ~ "Prix d'achat (CFA)",
      .x == "Marge...." ~ "Marge (%)",
      .x == "Prix.de.vente..CFA." ~ "Prix de vente (CFA)",
      .x == "Bénéfice..CFA." ~ "Bénéfice (CFA)",
      TRUE ~ .x
    ))
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
    
    prix_achat <- round((1 + input$transport) * input$prix_shein * input$taux_cfa / 100) * 100 
    prix_vente <- round((1 + input$marge) * prix_achat / 100) * 100 
    benefice <- prix_vente - prix_achat
    
    data <- articles()
    
    data[sel, ] <- list(
      input$nom,
      format(as.POSIXct(Sys.time(), tz = "Europe/Paris"), "%d-%m-%Y %H:%M:%S"),
      input$prix_shein,
      input$taux_cfa,
      input$transport,
      round(prix_achat / 100) * 100,
      # round(prix_achat, 0),
      input$marge,
      round(prix_vente / 100) * 100,
      # round(prix_vente, 0),
      # round(benefice / 100) * 100,
      benefice
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
      data <- articles()
      data <- data %>%
        rename_with(~ case_when(
          .x == "Prix.Shein...." ~ "Prix Shein (€)",
          .x == "Taux.CFA" ~ "Taux CFA",
          .x == "Transport...." ~ "Transport (%)",
          .x == "Prix.d.achat..CFA." ~ "Prix d'achat (CFA)",
          .x == "Marge...." ~ "Marge (%)",
          .x == "Prix.de.vente..CFA." ~ "Prix de vente (CFA)",
          .x == "Bénéfice..CFA." ~ "Bénéfice (CFA)",
          TRUE ~ .x
        ))
      #print(colnames(data))
      data <- adorn_totals(data, where = "row",,,,
                           "Prix Shein (€)", 
                           "Prix d'achat (CFA)",
                           "Prix de vente (CFA)", 
                           "Bénéfice (CFA)")
      
    }
    # Affichage du tableau avec DT
    datatable(
      data,
      rownames = FALSE,  # pas de numéros de ligne
      selection = "single",  # sélection d’une seule ligne à la fois
      colnames = c(
        "Nom",
        "Date",
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
        buttons = list(
          list(extend = "excel", text = "📥 Exporter en Excel"),
          list(extend = "pdf", text = "📄 Exporter en PDF")
        ),
        columnDefs = list(list(className = "dt-center", targets = "_all")), # centre toutes les colonnes
        searching = FALSE,
        language = list(
          lengthMenu = "Afficher _MENU_ articles",
          zeroRecords = "Aucun article trouvé",
          info = "Affichage de _START_ à _END_ sur _TOTAL_ articles",
          infoEmpty = "Aucun article disponible",
          infoFiltered = "(filtré à partir de _MAX_ articles totales)",
          search = "Rechercher :",
          paginate = list(
            previous = "Précédent",
            'next' = "Suivant"
          ),
          buttons = list(
            excel = "Exporter en Excel",
            pdf = "Exporter en PDF"
          )
        )
      ),
      extensions = 'Buttons'
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
