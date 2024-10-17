# Librerías
libraries <- c("shiny", "FactoMineR", "shinyWidgets", "shinydashboard", "xtable", "openxlsx", "factoextra", "ggthemes", "tvthemes", "dplyr", "tidyverse", "DT","htmlwidgets")

# Instala los paquetes si no están instalados
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)

# Cargar librerías
lapply(libraries, library, character.only = TRUE)

# Cargar funciones adicionales si es necesario
source("cluster.carac.R")
source("escalar.R")
source("dellNULMarg.R")

# Define la interfaz de usuario de Shiny

  # Encabezado --------------------------------------------------------------
  header <- dashboardHeader( title="Asociaciones significativas de una Tabla de Contingencia" )

  # Sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Tabla de Datos Original", tabName = "datos_ori", icon = icon("table")),
      menuItem("Caracterización Valor de Test", tabName = "texto", icon = icon("list-alt"))
    )
    #,
    #actionButton("exit_btn", "Salir")
  )
  
  # Cuerpo ------------------------------------------------------------------
  body <- dashboardBody(
    tabItems(
        tabItem(
          tabName = "datos_ori",
          h3("Carga de fichero CSV"), 
          box(width = 12,
            h5("Verificar notación científica en datos numéricos"),   
            h5("Verificar y corregir caracteres especiales como acentos, la letra ü y la letra ñ "),   
            h5("Verificar identificadores de fila únicos"),  
            h5("------------------------"), 
            h5("------------------------"), 
            h3("OPCIONES para la carga del CSV"), 
            materialSwitch(inputId = "header", "El archivo tiene encabezado", value = TRUE),
            radioButtons("sep", "Separador de campos:", 
                         choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), 
                         selected = ";"),
            radioButtons("dec", "Separador decimal:", 
                         choices = c(Coma = ",", Punto = "."), 
                         selected = '.'),
            h3(" "), 
            materialSwitch(inputId = "escalar", "¿Desea escalar la Tabla?", value = TRUE),
            # Seleccionar el archivo CSV
            fileInput("file", "Selecciona un archivo CSV:", accept = ".csv"),
            h3(" "), 
          ),
          h3("Tabla de Datos Original"), 
          box(width = 12,
                       DTOutput("originalTable")
          )
        ),
        tabItem(
           tabName = "texto",
           h3("Caracterización Valor de Test"),  
           fluidRow(
                 sidebarPanel(
                     materialSwitch("tipo_desc", "¿Agregacion por Por grupo de filas?", FALSE),  
                     conditionalPanel(
                       condition = "input.tipo_desc == true",
                         pickerInput(
                                   inputId = "grupo", 
                                   label = "Seleccione Variables Agrupacion:", 
                                   choices = NULL, 
                                   options = pickerOptions(
                                     actionsBox = TRUE, 
                                     size = 4,
                                     selectedTextFormat = "count > 3"
                                   ), 
                                   multiple = TRUE
                          )
                      ),
                     actionButton("describirBtn", "Describir"),
                 ),
         box(width = 12,
                downloadButton("download_tabla", "Descargar la tabla de contigencia escalada en CSV"),
                h3(" "),
                materialSwitch("con_afc", "¿Quieres realizar el AFCS?", TRUE),  
                conditionalPanel(
                   condition = "input.con_afc == true",
                   h4("Análsis Factorial de Correspondencias Simples AFCS"), 
                   plotOutput("afc"),
                   downloadButton("download_ca", "Descargar Resumen del AFCS")
                ),
                 h4("Tabla Asociaciones"), 
                 box(width = 6,imageOutput(outputId ="Vtest",height = 120)),
                 h6("Se realiza una prueba correspondiente a la distribución hipergeométrica y se calcula la probabilidad de observar un valor más extremo que el observado. Para cada fila (o categoría), se ordenan en orden ascendente de valor de Test de cada una de las columnas que caracterizan la fila (asociación)"),
                  h3(" "),
                  conditionalPanel(
                    condition = "input.tipo_desc == false",
                    downloadButton("download_descFiltxt", "Descargar Descripcion Filas en TXT"),
                    downloadButton("download_descColtxt", "Descargar Descripcion Columnas en TXT")
                  ),
                  conditionalPanel(
                    condition = "input.tipo_desc == true",
                    downloadButton("download_descGFiltxt", "Descargar Descripcion Grupo Filas en TXT")
                  ),
                 h3(" "),
                 box(width = 12,verbatimTextOutput("desc_results"))
          )
        )
      )
  )
)
  
  ## App completo ----------------------------------------------------------------
  ui <- dashboardPage(
    skin = "green",
    header,
    sidebar,
    body
  )
  

# Definir el servidor
server <- function(input, output) {
  
  # Inicializa una variable reactiva para almacenar datos (inicialmente NULL)
  datos <- reactiveVal(NULL)
  # Inicializa una variable reactiva para almacenar un dataframe (inicialmente NULL)
  df <- reactiveVal(NULL)
  # Inicializa una variable reactiva para almacenar factores (inicialmente NULL)
  factores <- reactiveVal(NULL)
  # Inicializa una variable reactiva para almacenar una tabla (inicialmente NULL)
  tc <- reactiveVal(NULL)
  # Inicializa una variable reactiva para almacenar otra tabla (inicialmente NULL)
  ttc <- reactiveVal(NULL)
  # Inicializa una variable reactiva para almacenar resultados (inicialmente NULL)
  resu <- reactiveVal(NULL)
  # Inicializa una variable reactiva para almacenar una lista de resultados
  resultados <- reactiveVal(list())
  # Inicializa un vector para almacenar grupos (inicialmente vacío)
  grupo <- c()
  # Inicializa un vector para almacenar índices (inicialmente vacío)
  index <- c()
  
  observeEvent(input$file, {
    req(input$file) # Asegurar que un archivo esté cargado
    tryCatch({  
        df(read.csv2(input$file$datapath, 
                       fileEncoding = "UTF-8", # especificar la codificación de caracteres
                       #locale = locale(encoding = "UTF-8")) %>%  type_convert() %>%  mutate(id = if_else(duplicated(id), paste(id, row_number(), sep = "_"), row.names),
                       header = input$header, 
                       row.names = 1,
                       dec = input$dec, 
                       sep = input$sep, 
                       stringsAsFactors=TRUE
                       ))
        # Separar factores de números  
        num <-df()[, sapply(df(), function(x) is.numeric(x) && !is.factor(x)), drop=FALSE]
        
        # Identificar columnas de factores
        fact <- df()[, sapply(df(), is.factor), drop=FALSE]
        names(fact) <- names(df())[sapply(df(), is.factor)]
        factores((fact))
        
        # Actualizar opciones del menú desplegable
        updatePickerInput(getDefaultReactiveDomain(), inputId = "grupo", choices = names(fact))
        
        datos(as.data.frame(num))
        
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en la carga del fichero",
        "No se pudo cargar el fichero, revise formato del CSV y los parámetros ingresados",
        easyClose = TRUE
      ))
    })
  })
  
  output$originalTable <- DT::renderDataTable({
    DT::datatable(df(), options = list(scrollX = TRUE))  # scrollX permitir el desplazamiento horizontal
  })
  
  # Función para calcular la suma-producto
  suma_producto_func <- function(x) {
    sum(x) - prod(x)
    # sum(x) calcula la suma de todos los elementos en el vector x
    # prod(x) calcula el producto de todos los elementos en el vector x
    # La función retorna la diferencia entre la suma y el producto
  }
  
  # Función para realizar la agregación
  agregar_datos <- function(df, factores, numericas, funcion_agregacion) {
    df %>%
      group_by(across(all_of(factores))) %>%
      summarise(across(all_of(numericas), funcion_agregacion), .groups = 'drop')
    # df: El dataframe de entrada que contiene los datos a agregar
    # factores: Un vector de nombres de columnas que se usarán para agrupar los datos
    # numericas: Un vector de nombres de columnas numéricas que se agregarán
    # funcion_agregacion: La función de agregación que se aplicará a las columnas numéricas
    
    # group_by(across(all_of(factores))): Agrupa el dataframe por las columnas especificadas en factores
    # summarise(across(all_of(numericas), funcion_agregacion), .groups = 'drop'):
    #   - summarise: Crea un nuevo dataframe que contiene una fila por cada grupo
    #   - across(all_of(numericas), funcion_agregacion): Aplica la función de agregación a cada columna numérica
    #   - .groups = 'drop': Indica que no se mantengan los grupos en el resultado final
  }
  
  
  observeEvent(input$describirBtn, {
    tryCatch({
          if (!is.null(tc)) {
            tryCatch({
              # Valor de Test
              if (input$tipo_desc) {
                #Obtengo la tabla agregada en función de los factores seleccionados
                # Obtener numero de la variables seleccionadas
                grupo <- match(input$grupo, names(factores()))
                nombre_fac <-c(colnames(factores()[grupo]))
                print(nombre_fac)
                nombre_num <- c(colnames(datos()))
                #Realizo la agregación mediante la suma algebraica
                agrega <- as.data.frame(agregar_datos(df(),
                                                      nombre_fac,
                                                      nombre_num,
                                                      suma_producto_func))  
                # Combinar etiquetas para darle al dataframe un rowname
                rownames(agrega) <- apply(agrega[, nombre_fac, drop = FALSE], 1, 
                                          function(x) paste(x, collapse = "_"))
                # Eliminar las columnas originales de factores si es necesario
                agrega<-agrega[, !(names(agrega) %in% nombre_fac)]
                
                #Escalo la tabla agregada
                tc_agrega <- escalar(agrega, 0,100)
                #Elimino marginales nulos
                tc(as.data.frame.matrix(dellNULMarg(tc_agrega)))
                
                # Describir Asociaciones de grupos de filas en función de columnas
                d0 <- cluster.carac(tc(),row.names(tc()),"fr", neg=FALSE, 1.65)
                resultados(list(t0="Descripción grupos filas en función columnas",grupo=d0))
                
              } else {
                if(input$escalar){
                  tcx <-escalar(datos(), 0, 100) # Escalo la tabla agregada
                }else{
                  tcx <- datos()
                }
                tc((dellNULMarg(tcx))) # elimino marginales nulos
                d1 <- cluster.carac(tc(),row.names(tc()),"fr", neg=FALSE, 1.65)
                
                ttc(dellNULMarg(t(tc())))  # transpuesta
                d2 <- cluster.carac(ttc(),row.names(ttc()),"fr", neg=FALSE, 1.65)
                resultados(list(t1="Descripción filas en función columnas", filas = d1, 
                                t2="Descripción columnas en función filas", columnas = d2))
              }
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error en cálculo del Valor de Test",
                paste("Se ha producido un error al aplicar el Demod de frecuencias:", e$message),
                easyClose = TRUE
              ))
            })
          }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error al analizar la tabla",
        "Revise la tabla y asegure no tener datos faltantes o problemas de formato",
        easyClose = TRUE
      ))
      resultados(list(e=paste("Error al analizar la tabla:", e$message, "Revise la tabla y asegure no tener datos faltantes o problemas de formato")))
    })
    
      output$Vtest <- renderImage({
        # Devolver la información de la imagen
        ruta_relativa <- file.path("imagenes", "sigVtest.png")
        list(src = ruta_relativa,
             contentType = "png",
             width = 250,
             height = 120)
      }, deleteFile = FALSE)
      
      # Resultados Vtest en cuadro de texto
      output$desc_results <- renderPrint({
        req(resultados())
        d <- resultados()
        d
        if (!is.null(d$e)) {
          # d$e es un mensaje de error o información similar
          d$e
        } else {
          if (input$tipo_desc) {
            cat(d$t0,"\n")  # Título o encabezado del análisis
            d$grupo
          } else {
            cat(d$t1,"\n")
            print(d$filas)
            cat("-----------------------","\n")
            cat(d$t2,"\n")
            print(d$columnas)
          }
        }
      })
      
      output$afc <- renderPlot({
        contigencia <- tc()
        tryCatch({
            res2 <- FactoMineR::CA( contigencia,
                                    graph=FALSE ) 
            resu(res2)
            return( factoextra::fviz_ca_biplot(res2, 
                                               col.col = "darkblue",
                                               col.row = "darkred",
                                               labelsize = 4, #Ajusta el tamaño de las etiquetas
                                               pointsize = 5, # Tamaño del punto
                                               quali.sup = index,
                                               alpha.row="cos2", # Trasparencia del punto en función de cos2
                                               alpha.col="cos2", # Trasparencia del punto en función de cos2
                                               repel = T) + theme_gray()
            )
         
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error en cálculo de AFCS",
            paste("Se ha producido un error al realizar el Análisis de Correspondencia y visualización del plano factorial:", e$message),
            easyClose = TRUE
          ))
        })
      })
     
      # Guardar los resultados de la descripcion en fichero
      nombre <- input$file
         
     output$download_descFiltxt <- downloadHandler(
        filename = function() {
          paste(nombre, "_DescFil.txt")
        },
        content = function(file3) {
          if (!input$tipo_desc) {
            # Guarda cada data frame en un fichero de texto
            d <- resultados()
            t <- capture.output({
              if (input$tipo_desc) {
                cat(d$t0,"\n")  # Título o encabezado del análisis
                # Asumiendo que d$grupo es una lista de estructuras de datos (como data.frames) para cada clase
                if (!is.null(d$grupo)) {
                  print(d$grupo)  # Aquí ajustas la impresión según la estructura real de d$grupo
                }
              } else {
                cat(d$t1,"\n")
                if (!is.null(d$filas)) {
                  print(d$filas)
                }
              }
            })
            writeLines(t, file3 )
          }
        }
      )
     
    output$download_descColtxt <- downloadHandler(
        filename = function() {
          paste(nombre, "_DesCol.txt")
        },
        content = function(file5) {
          if (!input$tipo_desc) {
            # Guarda cada data frame en un fichero de texto
            d <- resultados()
            t <- capture.output({
              if (input$tipo_desc) {
                cat(d$t0,"\n")  # Título o encabezado del análisis
                # Asumiendo que d$grupo es una lista de estructuras de datos (como data.frames) para cada clase
                if (!is.null(d$grupo)) {
                  print(d$grupo)  # Aquí ajustas la impresión según la estructura real de d$grupo
                }
              } else {
                cat(d$t2,"\n")
                if (!is.null(d$columnas)) {
                  print(d$columnas)
                }
              }
            })
            
            writeLines(t, file5 )
          }
        }
      )      
   
      output$download_descGFiltxt <- downloadHandler(
        filename = function() {
          paste(nombre, "_DescGrupFil.txt")
        },
        content = function(file7) {
          # Guarda cada data frame en un fichero de texto
          if (input$tipo_desc) {
            # Guarda cada data frame en un fichero de texto
            d <- resultados()
            t <- capture.output({
              if (input$tipo_desc) {
                cat(d$t0,"\n")  # Título o encabezado del análisis
                # Asumiendo que d$grupo es una lista de estructuras de datos (como data.frames) para cada clase
                if (!is.null(d$grupo)) {
                  print(d$grupo)  # Aquí ajustas la impresión según la estructura real de d$grupo
                }
              } else {
                cat(d$t1,"\n")
                if (!is.null(d$filas)) {
                  print(d$filas)
                }
                cat(d$t2,"\n")
                if (!is.null(d$columnas)) {
                  print(d$columnas)
                }              }
            })
            writeLines(t, file7 )
          }
        }
      )
     
      output$download_ca <- downloadHandler(
        filename = function() {
          paste(nombre, "_AFCS.txt")
        },
        content = function(fileAFCS) {
          # En 'resu()' resultado de tu análisis de correspondencia
          resAFC <- resu()
          eig.val <- get_eigenvalue(resAFC)
          row <- get_ca_row(resAFC)
          col <- get_ca_col(resAFC)
          # Crear un data frame con los resultados
            t <- capture.output(print("Eigen Values"), print(round(eig.val,2)), print("\n"),
                                print("Coordenadas Filas"), print(round(row$coord,2)), print("\n"),
                                print("Coordenadas Columnas"), print(round(col$coord,2)),print("\n"),
                                print("Cosenos cuadrados Filas"), print(round(row$cos2,2)), print("\n"),
                                print("Cosenos cuadrados Columnas"), print(round(col$cos2,2))
                       )
          # Escribir los resultados en un archivo de texto
             writeLines(t, fileAFCS)
        }
      )
    })   
  
  # Guardar las tablas escaladas
  output$download_tabla <- downloadHandler(
    filename = function() {
      paste("TablaEscalada_",input$file$name)
    },
    content = function(file1) {
      write.csv2(tc(), file1, row.names = TRUE)
    }
  )
  
 # Detiene la aplicación Shiny cuando se presiona el botón de salida
  #observeEvent(input$exit_btn, {
  #  stopApp()  
  #})
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)


