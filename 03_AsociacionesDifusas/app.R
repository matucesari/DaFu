# app.R

# ─── Librerías ──────────────────────────────────────────────────────────────
libraries <- c(
  "shiny", "caret", "FactoMineR", "shinyWidgets", "shinydashboard", "xtable",
  "openxlsx", "factoextra", "ggthemes", "tvthemes", "dplyr", "tidyverse",
  "DT", "htmlwidgets"
)
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)
lapply(libraries, library, character.only = TRUE)

# Cargar funciones auxiliares
source("cluster.carac.R")
source("escalar.R")
source("dellNULMarg.R")


# ─── UI ─────────────────────────────────────────────────────────────────────
header <- dashboardHeader(title = "Asociaciones significativas de una Tabla de Contingencia")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tabla de Datos Original",       tabName = "datos_ori", icon = icon("table")),
    menuItem("Caracterización Valor de Test", tabName = "texto",     icon = icon("list-alt"))
  )
)

body <- dashboardBody(
  tabItems(
    
    # ── Pestaña datos_ori ─────────────────────────────────────────────────────
    tabItem("datos_ori",
            h3("Carga de fichero CSV"),
            box(width = 12,
                h5("Verificar notación científica, acentos, ü, ñ y IDs únicos"),
                h3("OPCIONES para la carga del CSV"),
                materialSwitch("header",  "El archivo tiene encabezado",          TRUE),
                radioButtons("sep",       "Separador de campos:",
                             choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"),
                             selected = ";"),
                radioButtons("dec",       "Separador decimal:",
                             choices = c(Coma = ",", Punto = "."), selected = "."),
                materialSwitch("escalar", "¿Desea escalar la Tabla?",             TRUE),
                fileInput("file",         "Selecciona un archivo CSV:", accept = ".csv")
            ),
            h3("Tabla de Datos Original"),
            box(width = 12, DTOutput("originalTable"))
    ),
    
    # ── Pestaña texto ──────────────────────────────────────────────────────────
    tabItem("texto",
            h3("Caracterización Valor de Test"),
            fluidRow(
              sidebarPanel(
                materialSwitch("tipo_desc","¿Agregación por grupo de filas?", FALSE),
                conditionalPanel(
                  "input.tipo_desc == true",
                  pickerInput("grupo","Variables Agrupación:",
                              choices = NULL,
                              options = pickerOptions(actionsBox=TRUE,size=4),
                              multiple = TRUE)
                ),
                actionButton("describirBtn","Describir")
              ),
              
              box(width = 12,
                  downloadButton("download_tabla",     "Descargar tabla escalada CSV"),
                  h3(" "),
                  materialSwitch("con_afc","¿Quieres realizar el AFCS?", TRUE),
                  conditionalPanel(
                    "input.con_afc == true",
                    h4("Análisis Factorial de Correspondencias Simples AFCS"),
                    plotOutput("afc"),
                    downloadButton("download_ca","Descargar Resumen AFCS")
                  ),
                  h4("Tabla Asociaciones"),
                  box(width=6, imageOutput("Vtest", height=120)),
                  h6("Se realiza prueba hipergeométrica y se ordenan asociaciones por Test-Value"),
                  h3(" "),
                  conditionalPanel(
                    "input.tipo_desc == false",
                    downloadButton("download_descFiltxt","Descargar Descripción Filas TXT"),
                    downloadButton("download_descColtxt","Descargar Descripción Columnas TXT")
                  ),
                  conditionalPanel(
                    "input.tipo_desc == true",
                    downloadButton("download_descGFiltxt","Descargar Descripción Grupo Filas TXT")
                  ),
                  downloadButton("download_csv_combinado","Descargar CSV Combinado"),
                  downloadButton("download_excel_freq",    "Descargar frecuencias Excel"),
                  h3(" "),
                  box(width = 12, verbatimTextOutput("desc_results"))
              )
            )
    )
    
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)


# ─── SERVER ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  datos      <- reactiveVal(NULL)
  df         <- reactiveVal(NULL)
  factores   <- reactiveVal(NULL)
  tc         <- reactiveVal(NULL)
  ttc        <- reactiveVal(NULL)
  resultados <- reactiveVal(list())
  
  # para retener la tabla de frecuencias antes de exportar
  last_freq <- reactiveVal(data.frame())
  
  # Carga CSV y actualiza pickers
  observeEvent(input$file, {
    req(input$file)
    D <- read.csv2(input$file$datapath,
                   header      = input$header,
                   sep         = input$sep,
                   dec         = input$dec,
                   stringsAsFactors = TRUE,
                   row.names   = 1,
                   fileEncoding = "UTF-8")
    df(D)
    datos(D[, sapply(D, is.numeric), drop = FALSE])
    factDF <- D[, sapply(D, is.factor), drop = FALSE]
    factores(factDF)
    updatePickerInput(session, "grupo", choices = names(factDF))
  })
  
  # Mostrar datos originales
  output$originalTable <- renderDT({
    req(df())
    datatable(df(), options = list(scrollX = TRUE))
  })
  
  # Renderizar imagen V-test
  output$Vtest <- renderImage({
    list(src = file.path("imagenes","sigVtest.png"),
         contentType = "image/png", width = 250, height = 120)
  }, deleteFile = FALSE)
  
  # Describir (cluster.carac + AFCS + resultados)
  observeEvent(input$describirBtn, {
    req(df())
    
    # … lógica original para calcular tc(), d0 o d1/d2, resultados() …
    if (input$tipo_desc) {
      # agrupación por variables de grupo
      grupos_idx <- match(input$grupo, names(factores()))
      nom_fac    <- names(factores())[grupos_idx]
      agrega <- as.data.frame(
        df() %>%
          group_by(across(all_of(nom_fac))) %>%
          summarise(across(where(is.numeric), ~ sum(.) - prod(.)), .groups='drop')
      )
      rownames(agrega) <- apply(agrega[, nom_fac, drop=FALSE], 1, paste, collapse="_")
      agrega <- agrega[, setdiff(names(agrega), nom_fac), drop = FALSE]
      tc(escalar(agrega,0,100) %>% dellNULMarg())
      d0 <- cluster.carac(tc(), row.names(tc()), "fr", neg = FALSE, 1.65)
      resultados(list(t0 = "Descripción grupos filas en función columnas", grupo = d0))
    } else {
      # descripción filas/columnas
      tabx <- if (input$escalar) escalar(datos(),0,100) else datos()
      tc(dellNULMarg(tabx))
      d1 <- cluster.carac(tc(), row.names(tc()), "fr", neg = FALSE, 1.65)
      ttc(dellNULMarg(t(tc())))
      d2 <- cluster.carac(ttc(), row.names(ttc()), "fr", neg = FALSE, 1.65)
      resultados(list(
        t1 = "Descripción filas en función columnas", filas = d1,
        t2 = "Descripción columnas en función filas", columnas = d2
      ))
    }
    
    # AFCS
    if (input$con_afc) {
      output$afc <- renderPlot({
        resAFC <- FactoMineR::CA(tc(), graph = FALSE)
        factoextra::fviz_ca_biplot(resAFC,
                                   col.col = "darkblue",
                                   col.row = "darkred",
                                   labelsize = 4,
                                   pointsize = 5,
                                   repel = TRUE) +
          ggthemes::theme_tufte()
      })
      output$download_ca <- downloadHandler(
        filename = function() paste0(tools::file_path_sans_ext(input$file$name), "_AFCS.txt"),
        content = function(f) {
          resAFC <- FactoMineR::CA(tc(), graph = FALSE)
          eig  <- factoextra::get_eigenvalue(resAFC)
          rowc <- factoextra::get_ca_row(resAFC)
          colc <- factoextra::get_ca_col(resAFC)
          t <- capture.output({
            print("Eigenvalues:");    print(round(eig,2))
            print("Row coords:");     print(round(rowc$coord,2))
            print("Column coords:");  print(round(colc$coord,2))
          })
          writeLines(t, f)
        }
      )
    }
    
    # Imprimir resultados en pantalla
    output$desc_results <- renderPrint({
      res <- resultados()
      if (input$tipo_desc) {
        cat(res$t0, "\n"); print(res$grupo)
      } else {
        cat(res$t1, "\n"); print(res$filas)
        cat("-------\n"); cat(res$t2, "\n"); print(res$columnas)
      }
    })
    
    # Construir tabla de frecuencias para exportar
    freq_df <- data.frame()
    res <- resultados()
    if (input$tipo_desc) {
      for(cl in names(res$grupo)) {
        tab <- res$grupo[[cl]]
        if (is.data.frame(tab)) {
          tmp <- data.frame(
            class = cl,
            characteristic = rownames(tab),
            Test.Value     = tab[,"Test.Value"],
            stringsAsFactors = FALSE
          )
          freq_df <- bind_rows(freq_df, tmp)
        }
      }
    } else {
      for(cl in names(res$filas)) {
        tab <- res$filas[[cl]]
        tmp <- data.frame(
          class = cl,
          characteristic = rownames(tab),
          Test.Value     = tab[,"Test.Value"],
          stringsAsFactors = FALSE
        )
        freq_df <- bind_rows(freq_df, tmp)
      }
      for(cl in names(res$columnas)) {
        tab <- res$columnas[[cl]]
        tmp <- data.frame(
          class = cl,
          characteristic = rownames(tab),
          Test.Value     = tab[,"Test.Value"],
          stringsAsFactors = FALSE
        )
        freq_df <- bind_rows(freq_df, tmp)
      }
    }
    last_freq(freq_df)
  })
  
  
  # ── Descarga de la tabla de contingencia escalada
  output$download_tabla <- downloadHandler(
    filename = function() paste0("TablaEscalada_", input$file$name),
    content  = function(f) write.csv2(tc(), f, row.names = TRUE)
  )
  
  # ── Descargas TXT (descripciones) ──────────────────────────────────────────
  output$download_descFiltxt <- downloadHandler(
    filename = function() paste0(input$file$name, "_DescFil.txt"),
    content  = function(f) {
      res <- resultados()
      t <- capture.output({
        cat(res$t1, "\n"); print(res$filas)
      })
      writeLines(t, f)
    }
  )
  output$download_descColtxt <- downloadHandler(
    filename = function() paste0(input$file$name, "_DescCol.txt"),
    content  = function(f) {
      res <- resultados()
      t <- capture.output({
        cat(res$t2, "\n"); print(res$columnas)
      })
      writeLines(t, f)
    }
  )
  output$download_descGFiltxt <- downloadHandler(
    filename = function() paste0(input$file$name, "_DescGrp.txt"),
    content  = function(f) {
      res <- resultados()
      t <- capture.output({
        cat(res$t0, "\n"); print(res$grupo)
      })
      writeLines(t, f)
    }
  )
  
  # ── CSV combinado (class, characteristic, Test.Value) ─────────────────────
  output$download_csv_combinado <- downloadHandler(
    filename = function() paste0(input$file$name, "_combinado.csv"),
    content = function(f) {
      freq_df <- last_freq()
      write.csv2(freq_df, f, row.names = FALSE)
    }
  )
  
  # ── NUEVO: Descargar frecuencias en Excel ───────────────────────────────────
  output$download_excel_freq <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "_frecuencias.xlsx")
    },
    content = function(path) {
      req(last_freq())
      write.xlsx(
        list(Frecuencias = last_freq()),
        path,
        rowNames = FALSE
      )
    }
  )
}

shinyApp(ui, server)
