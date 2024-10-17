# Carga las librerías necesarias para el proyecto
libraries <- c("shiny", "shinydashboard", "shinycssloaders", 
               "FuzzyR", "shinyWidgets",  "dplyr", "tidyr",
               "classInt", "DataExplorer", "RColorBrewer", 
               "sparkline", "DT","htmlwidgets","ggplot2","GGally")

# Instala automáticamente los paquetes 
# que no están instalados aún y resuelve sus dependencias
install.packages(setdiff(libraries, 
                         rownames(installed.packages())), 
                 dependencies = TRUE)

# Carga las librerías especificadas en la lista anterior
lapply(libraries, library, character.only = TRUE)

# Define la interfaz de usuario (UI) 
# de la aplicación Shiny usando 'shinydashboard'

# Encabezado de la página con un título específico
header <- dashboardHeader(
  title="Crear Conjuntos difusos a partir de partición en Clases Óptimas")

# Barra lateral de navegación que contiene menús 
# para diferentes secciones de la aplicación
sidebar <- dashboardSidebar(
  # Menú en la barra lateral con elementos 
  # que los usuarios pueden seleccionar
  sidebarMenu(
    # Elemento de menú para cargar datos 
    # con un icono de tabla
    menuItem("Cargar la Tabla de Datos", 
             tabName = "datos", 
             icon = icon("table")),
    # Elemento de menú para acceder a la sección de 
    # partición y lógica difusa con un icono de lista
    menuItem("Partición y LD", 
             tabName = "parti", 
             icon = icon("list-alt"))
  )
  #,
  # Botón para salir de la aplicación
  # actionButton("exit_btn", "Salir")
)

# Cuerpo de la interfaz de usuario, que contiene los elementos visuales
# que los usuarios interactuarán directamente
body <- dashboardBody(
  # Define los elementos en pestañas para organización y navegación
  tabItems(
    # Pestaña para la carga y visualización de datos
    tabItem(
      tabName = "datos",
      # Título de la sección
      h2("Carga de fichero CSV"),
      # Contenedor con ajustes de configuración 
      # para la carga del archivo
      box(width = 12,
          # Instrucciones para el usuario
          h5("La tabla CSV DEBE tener en 1º coluna: etiqueta de observaciones ID"), 
          # Interruptores para especificar características del archivo CSV
          materialSwitch(inputId = "header", "El archivo tiene Encabezado", value = TRUE),
          materialSwitch(inputId = "nominales", "La tabla contiene Variables Categóricas Nominales", value = FALSE),
          # Opciones para especificar el formato del archivo CSV
          radioButtons("sep", "Separador de columnas:", choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), selected = ";"),
          radioButtons("dec", "Separador decimal:", choices = c(Coma = ",", Punto = "."), selected = '.'),
          fileInput("file", "Selecciona un archivo CSV:", accept = ".csv")
      ),
      h2("Datos Originales"),
      # Visualización de estadísticas y datos originales
      box(width = 12,
          h3("Estadísticas de las Variables Cuantitativas"),
          verbatimTextOutput("estadi"),
          box(width = 12, 
              plotOutput("corre"),
              conditionalPanel(
                condition = "input.nominales == true",
                selectInput("catVar", "Seleccione la variable categórica para Colorear Grafico:", choices = NULL)
              )
          ),
          conditionalPanel(
            condition = "input.nominales == true",
            h3("Estadísticas de las Variables Cualitativas"),
            plotOutput("bar_char")
          )
      ),
      h3("Tabla de Datos"),
      DTOutput("originalTable", height = 700)
    ),
    # Pestaña para la partición y borrosificación de datos
    tabItem(
      tabName = "parti",
      h2("Partición Univariada"),
      box(width = 12,
          box(width = 4,
              # Configuraciones para la discretización de una variable
              selectInput("variable", 
                          "Variable a discretizar:", 
                          choices = NULL),
              numericInput("num_intervals", 
                           "Número de intervalos de clase:", 
                           value = 5, min = 2),
              radioButtons("method", 
                           "Método de clasificación:", 
                           choices = c("Fisher", "K-means"), 
                           selected = "Fisher"),
              numericInput("num_dec", 
                           "Número de decimales conservar:", 
                           value = 1, min = 0),
              # Botones y opciones adicionales
              actionButton("discretize_btn", "Discretizar"),
              h5("Elija tipo de función de pertenecia"),
              awesomeRadio(
                inputId = "tipo_set",
                label = "Radio buttons",
                choices = c("Gaussiana", "Triangular"),
                inline = TRUE,
                checkbox = TRUE
              )
          ),
          h3("Discretización"),
          verbatimTextOutput("discretization_results"),
          box(tableOutput("disc_intervalos")),
          downloadButton("downloadData", "Descargar Resultados"),
          box(width = 12,plotOutput("var_plot")),
          h3(" "),
          column(6, actionButton("fuzzy_btn", "Borrosificar")),
          h3(" "),
          box(width = 12,
              h3("Tabla de Datos Borrosificada"),
              downloadButton("download_Xls", "Descargar datos borrosificados en XLSx"),
              DTOutput("fuzzy_table")
          )
      )
    )
  )
)

# Definición del UI completo de la aplicación Shiny, incluyendo encabezado, barra lateral y cuerpo
ui <- dashboardPage(
  skin = "green",  # Establece el color de tema para la interfaz
  header,
  sidebar,
  body
)

# Define una función llamada 'gaussiana' que calcula el valor de la función de pertenencia Gaussiana.
# Parámetros:
#   x: el valor de entrada para calcular su grado de pertenencia.
#   a: el centro de la curva Gaussiana, donde el grado de pertenencia es máximo.
#   b: la desviación estándar, controla el ancho de la campana de la curva.
# Devuelve:
#   El grado de pertenencia de 'x' en la función Gaussiana, calculado con la fórmula de la densidad de probabilidad normal.
gaussiana <- function(x, a, b) {
  return(exp(-((x - a) / b)^2))
}

# Define una función llamada 'triangular' que calcula el valor de la función de pertenencia Triangular.
# Parámetros:
#   x: el valor de entrada para calcular su grado de pertenencia.
#   a: el inicio de la base de la curva triangular.
#   b: el punto en el que el grado de pertenencia alcanza su máximo (el vértice superior del triángulo).
#   c: el final de la base de la curva triangular.
# Devuelve:
#   El grado de pertenencia de 'x' en la función Triangular, usando cálculos de mínimos y máximos para formar la forma triangular.
# La función 'pmin' y 'pmax' se usan para asegurar que el valor devuelto esté correctamente acotado entre 0 y 1, y forme una línea ascendente y descendente adecuada entre 'a', 'b' y 'c'.
triangular <- function(x, a, b, c) {
  return(pmax(
    pmin(
      (x-a)/(b-a),  # Calcula la pendiente ascendente desde 'a' hasta 'b'
      (c-x)/(c-b)  # Calcula la pendiente descendente desde 'b' hasta 'c'
    ), 
    0)  # Asegura que el valor mínimo es 0
  )
}

# Define la función del servidor para la aplicación Shiny, 
# que maneja la lógica y la interactividad de la app.
server <- function(input, output) {
  # Inicialización de variables reactivas, usadas para almacenar y 
  # actualizar datos dinámicamente a lo largo de la sesión.
  datos <-  reactiveVal(list())
  factores <- reactiveVal(NULL)
  df <- reactiveVal(NULL)
  e <- reactiveVal(NULL)
  historial <- reactiveVal(list())
  r <- reactiveVal(NULL)
  tabla_resultados <- reactiveVal(NULL)
  
  # Observa eventos del input de archivo CSV y procesa el archivo cargado
  observeEvent(input$file, {
    tryCatch({
      # Carga el archivo CSV y actualiza el dataframe reactivo 'df'
        df( read.csv(input$file$datapath, 
                     header = input$header, 
                     dec = input$dec, 
                     sep = input$sep, 
                     stringsAsFactors = TRUE, 
                     row.names = 1 ))
      # Extrae columnas que son factores y numéricas separadamente, 
      # asegurando que no se caiga a un vector si hay una sola columna
      # Identificar columnas de factores
        fact <- as.data.frame(df()[, sapply(df(), is.factor), drop=FALSE])
      # Separar factores de números  
         n <-as.data.frame(df()[, sapply(df(), function(x) is.numeric(x)  && !is.factor(x)), drop = FALSE])
      # Para asegurar que el nombre de la variable se mantenga incluso cuando hay solo una columna, 
      # puedes usar el argumento drop = FALSE al extraer las columnas numéricas
      # Convierte columnas relevantes a numéricas si es necesario
         f <- as.data.frame(lapply(fact, as.double))
      # Actualiza la interfaz las listas de seleccion de variables
         updateSelectInput(getDefaultReactiveDomain(), inputId = "variable", choices = c(names(n),names(f)), selected = NULL) 
         updateSelectInput(getDefaultReactiveDomain(), inputId = "catVar", choices = c(names(fact),"Sin selección"), selected = "Sin selección") 
      # Guarda los factores y datos separados en sus respectivas variables reactivas
        factores(fact)
        datos(list(cuanti=n, cuali=f))
    }, error = function(e) {
      # Maneja errores en la carga del archivo mostrando un diálogo modal con el mensaje de error
      showModal(modalDialog(
        title = "Error en la Carga del fichero CSV ",
        paste("Se ha producido un error al cargar y preprocesamiento del fichero:  ", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Renderiza una tabla DataTable de los datos originales cargados
  output$originalTable <- DT::renderDataTable({
    req(input$file)
    DT::datatable(cbind(df()), options = list(scrollX = TRUE))
  })
  
  # Calcula y muestra estadísticas básicas de las variables cuantitativas
  output$estadi <- renderPrint({
    req(input$file)
    tryCatch({
      summary(datos()$cuanti)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en Calculo de Estadísitcas",
        paste("Se ha producido un error al intentar calcular las estadísitcas básicas: ", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Genera y muestra una matriz de correlación de variables cuantitativas
  output$corre <- renderPlot({
    req(input$file)
    tryCatch({
      # Matriz de correlación
      #plot_correlation(datos()$cuanti)
      # Ajustes según la entrada de la variable categórica
      if (input$nominales != FALSE && input$catVar != "Sin selección") {
        d <- factores()  # Rescato las variables categoricas
        colorVar <- input$catVar
        # Creación del gráfico Diagrama de pares con ggpairs
        ggpairs(datos()$cuanti,
                title = "Diagrama de pares", axisLabels = "show",
                aes(color = d[[colorVar]], alpha = 0.5),
                lower = list(continuous = "smooth") )
      } else {
        # Creación del gráfico Diagrama de pares con ggpairs
        ggpairs(datos()$cuanti,
                title = "Diagrama de pares", axisLabels = "show",
                lower = list(continuous = "smooth") )
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en cálculo de matriz de correlación ",
        paste("Se ha producido un error al realizar el cálculo y grafico de las correlaciones entre variables numéricas: ", e$message),
        easyClose = TRUE
      ))
    })
  }) 
  # Renderiza gráficos de barras para las distribuciones de las variables categóricas.
  output$bar_char <- renderPlot({
    req(input$nominales)
    if(input$nominales){
      # Gráfico de barras con xray::distributions(data frame factores)
      # xray::distributions(datos()$cuali)
      # Calcular frecuencias para cada factor y almacenar en una lista de tablas
      list_of_tables <- lapply(factores(), table)
      # Convertir la lista de tablas a un dataframe
      frequency_data <- bind_rows(lapply(names(list_of_tables), function(x) {
        data.frame(Factor = x,
                   Level = names(list_of_tables[[x]]),
                   Frequency = as.vector(list_of_tables[[x]]),
                   stringsAsFactors = FALSE)
      }), .id = "Variable")
      # Graficar las frecuencias usando ggplot2
      ggplot(frequency_data, aes(x = Level, y = Frequency, fill = Level)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ Factor, scales = "free_x") +
        theme_minimal() +
        labs(title = "Frecuencia ocurrencia de cada Variables Nominales",
             x = "Modalidades o Categorias",
             y = "Frecuencia") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Función para discretizar variables
  discretizar <- function(datos, variable, num_intervals, metodo, num_dec) {
    variable_name <- names(datos[variable])
    if (metodo == "Fisher") {  style <- "fisher"  } else {  style <- "kmeans"    }
    r_intervals <- classIntervals(datos[,variable], n = num_intervals, style = style, dataPrecision = num_dec)
    # Obtén el número de intervalos discretizados
    inter <- length(r_intervals$brks) - 1
    # Crea un vector de números de rango basado en el número de intervalos
    rangos_numericos <- 1:inter
    # Crear las etiquetas de las modalidades o rangos
    labels<-paste0(substr(variable_name, start = 1, stop = 3),
                     rangos_numericos,
                     "[",
                     round(r_intervals$brks[-num_intervals],num_dec), 
                     "a", 
                     round(r_intervals$brks[-1],num_dec),"]")
    e(labels)
    r(r_intervals$brks)
    return(capture.output(print(r_intervals)))
  }
  observeEvent(input$discretize_btn, {
        tc <- cbind(datos()$cuanti,datos()$cuali)
        # Discretizar la variable seleccionada
        x <- discretizar(tc, input$variable, input$num_intervals, input$method, input$num_dec)
        
        # Resultados de discretización en un cuadro de texto
        output$discretization_results <- renderText({
          req(input$discretize_btn)
          return(paste( x, collapse = "\n"))
        })
        
        # Detalle de los rangos
        output$disc_intervalos <- renderTable({
          req(r())
          limites <- r()  #vector con limites cerrados de rangos
          if(input$tipo_set == "Triangular"){
            # Funcion triangular
            tr <- data.frame(
              Conjunto = e(),
              Minimo = head(limites, -1), # Excluye el último elemento
			        Maximo = tail(limites, -1), # Excluye el primer elemento
              b = (limites[-length(limites)] + limites[-1]) / 2,  # Punto central del intervalo
              a = c(limites[1], head((limites[-length(limites)] + limites[-1]) / 2, -1)),  # Desfase de b para obtener a
              c = c(tail((limites[-length(limites)] + limites[-1]) / 2, -1), limites[length(limites)])  # Desfase de b para obtener c
            )
            primero <- 1
            ultimo <- length(limites)-1
            # Ajusto los extremos
            tr$b[primero] <- tr$a[primero]
            tr$b[ultimo] <- tr$c[ultimo]
          }else {
            # Funcion Gaussiana
            tr <- data.frame(
              Conjunto = e(),
              Minimo = head(limites, -1), # Excluye el último elemento
			        Maximo = tail(limites, -1), # Excluye el primer elemento
              # Calcular 'a' como el promedio de límites consecutivos
			        a = (head(limites, -1) + tail(limites, -1)) / 2,
			        # Calcular 'b' como la mitad de la diferencia entre límites consecutivos
			        b = (tail(limites, -1) - head(limites, -1)) / 2
            )
            primero <- 1
            ultimo <- length(limites)-1
            # Ajuste para el primer y último valor de 'a'
            tr$a[primero]<-tr$Minimo[primero]
            tr$a[ultimo]<-tr$Maximo[ultimo]
            tr$b[primero]<-tr$b[primero]*2
            tr$b[ultimo]<-tr$b[ultimo]*2
          }
          tabla_resultados(tr)
          tr
        }, rownames = TRUE)     
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("Conjuntos_", input$variable, ".txt", sep = "")
          },
          content = function(file) {
              # Obtener los datos que se mostrarán
              datos_disc = x
              datos_intervalos = tabla_resultados()
              # Escribir los datos en un archivo de texto
              sink(file)
              cat("Resultados de Discretización:\n")
              cat(datos_disc, "\n")
              cat("\nDetalles de los Rangos:\n")
              print(datos_intervalos)
              sink()  
          }
        )
  })
  
  # Grafico variable
  output$var_plot <- renderPlot({
    req(tabla_resultados())
    da <- as.data.frame(cbind(datos()$cuanti, datos()$cuali))
    variable_name <- names(da[input$variable])
    conjuntos <- tabla_resultados()
    gradiente <- nrow(conjuntos)
    min <- min(conjuntos$Minimo)
    max <- max(conjuntos$Maximo)
    x_vals <- seq(min, max, length.out = 10000)
    titulo  <- switch(input$tipo_set,
                      "Gaussiana" =  "Funciones Gaussianas para Conjuntos Difusos",
                      "Triangular" = "Función Triangular" 
    )
    # Ajustar el límite superior del eje y para dar más espacio a las etiquetas
    ylim_max <- 1.1
    # Crear una paleta de colores personalizados
    custom_palette <- c("red","darkred","darkviolet","darkblue", "darkgreen","royalblue",
                        "darkorange", "darkcyan", "magenta")
    # Asignar colores, repitiendo si es necesario
    colores <- rep(custom_palette, length.out = gradiente)
    # Creo el gráfico
    plot(x_vals, 
         rep(0, length(x_vals)), 
         type = "n", 
         xlab = "Valor de x", 
         ylab = "Función pertenencia", 
         ylim = c(0, 1),
         xlim = c(min,max),
         main = paste(titulo, variable_name, "\n")
    )
    # Agregar cada función al gráfico
    for (i in 1:gradiente) {
      y_vals <- switch(input$tipo_set,
                       "Gaussiana" =  gaussiana(x_vals, conjuntos[i, "a"], conjuntos[i, "b"]),
                       "Triangular" = triangular(x_vals, conjuntos[i, "a"], conjuntos[i, "b"], conjuntos[i, "c"]) 
      )
      lines(x_vals, y_vals, col = colores[i], lwd = 2)
    }
    # Truncar las etiquetas a los primeros 4 caracteres
    truncated_labels <- substr(e(), 1, 4)
    # Agregar la leyenda en una posición específica (hacia abajo, medio derecha)
    legend("topright", inset=c(0.05, 0.35), legend = truncated_labels, col = colores, lwd = 2, cex = 0.8)
    
  })
  
      observeEvent(input$fuzzy_btn, {
          req(tabla_resultados())
          da <- as.data.frame(cbind(datos()$cuanti, datos()$cuali))
          variable_name <- names(da[input$variable])
          
          conjuntos <- tabla_resultados()
          gradiente <- nrow(conjuntos)
          
          # Realizar la inferencia difusa
          if(input$tipo_set == "Gaussiana" ){
            inferencia_resultado <- data.frame(
              lapply(1:gradiente, function(i) {
                mf <- genmf('gaussmf', c(conjuntos$b[i], conjuntos$a[i]))
                round(evalmf(da[[variable_name]], mf), 4)
              })
            )
          }
          if(input$tipo_set == "Triangular" ){
            inferencia_resultado <- data.frame(
              lapply(1:gradiente, function(i) {
                mf <- genmf('trimf', c(conjuntos$a[i], conjuntos$b[i], conjuntos$c[i]))
                round(evalmf(da[[variable_name]], mf), 4)
              })
            )
          }
          # colnames(inferencia_resultado) <- paste0("C", 1:gradiente)
          etiquetas <- gsub("\\[", "_",  e()) # Reemplaza corchete abierto
          etiquetas <- sub("\\]", "_",  etiquetas) # Reemplaza corchete cerrado
          colnames(inferencia_resultado) <-  etiquetas
         
          historial(list(
            data_fuzzy = inferencia_resultado, # Datos borrosificados
            var_orig = da[input$variable],     # Variable original
            nombre_variable = variable_name    # Nombre de la variable
          ))
        })
  
   output$fuzzy_table <- DT::renderDataTable({
          req(historial()$data_fuzzy)
          result <- tryCatch(
            expr = {
              o <- historial()$var_orig
              f <- round(historial()$data_fuzzy,4)
              DT::datatable(data.frame(o,f), 
                            options = list(scrollX = TRUE))
            },
            error = function(e) {
              showModal(modalDialog(
                title = "Error en Visualización de Tabla con números borrosos",
                paste("Se ha producido un error en la visualización de resultados"),
                easyClose = TRUE
              ))
            }
          )
        })
        
        # Guardar los resultados de la borrosificación en fichero
   output$download_Xls <- downloadHandler(
          filename = function() {
            paste(historial()$nombre_variable, "_Fuzzy.csv")
          },
          content = function(file) {
            o <- historial()$var_orig
            f <-as.data.frame.matrix(historial()$data_fuzzy)
            if(!is.null(factores())){
              r <- data.frame(f,as.data.frame(factores()))
            }else{
              r <- data.frame(f)
            }
            
            # Ajusta el formato de los valores en notación científica y el separador decimal
            r[] <- lapply(r, function(x) {
              if (is.numeric(x)) {
                format(x, scientific = TRUE, digits = 15)
              } else {
                x
              }
            })
            # Guarda el Data Frame en un archivo CSV
            write.csv2(r, file, row.names = TRUE)
          }
        )
  # Detiene la aplicación Shiny cuando se presiona el botón de salida
   # observeEvent(input$exit_btn, {
   #  stopApp()  
   # })
}

  
# Ejecutar la aplicación Shiny
shinyApp(ui, server)
