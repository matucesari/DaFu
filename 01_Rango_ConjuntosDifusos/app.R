# Librerías
libraries <- c("shiny", "FuzzyR", "shinyWidgets", "shinydashboard", "DataExplorer","tidyr",
               "openxlsx", "dplyr", "xray", "DT","htmlwidgets","ggplot2","GGally")

# Instala los paquetes si no están instalados
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)

# Cargar librerías
lapply(libraries, library, character.only = TRUE)

# Define la interfaz de usuario de Shiny

  # Encabezado --------------------------------------------------------------
  header <- dashboardHeader( title="Definir conjuntos difusos y aplicar la función de membresía" )

  # Sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Tabla de Datos Original", tabName = "datos_ori", icon = icon("table")),
      menuItem("Borrosificación", tabName = "var_fuzzy", icon = icon("cog", lib = "glyphicon"))
    ),
    actionButton("exit_btn", "Salir")
  )
  
  # Cuerpo ------------------------------------------------------------------
  body <- dashboardBody(
  		tabItems(
  				tabItem(
  				  tabName = "datos_ori",
  				  h2("Carga de fichero CSV"), 
  				  box(width = 12,
  				      # Instrucciones para el usuario
  				      h5("La tabla CSV DEBE tener en 1º coluna: etiqueta de observaciones ID"), 
  				      # Interruptores para especificar características del archivo CSV
  				      materialSwitch(inputId = "header", "El archivo tiene encabezado", value = TRUE),
  				      materialSwitch(inputId = "nominales", "La tabla tiene Variables Categóricas Nominales", value = FALSE),
  				      # Opciones para especificar el formato del archivo CSV
  				      radioButtons("sep", "Separador de campos:", choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), selected = ";"),
  				      radioButtons("dec", "Separador decimal:", choices = c(Coma = ",", Punto = "."), selected = '.'),
  				      fileInput("file", "Selecciona un archivo CSV:", accept = ".csv")
  				  ),
  				  h2("Datos Originales"),
  				  # Visualización de estadísticas y datos originales
  				  box(width = 12,
  				      h3("Estadísitcas de las Variables Cuantitativas"),
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
  				        h3("Estadísitcas de las Variables Cualitativas"),
  				        plotOutput("bar_char")
  				      )
  				  ),
  				  h3("Tabla de Datos"),
  				  DTOutput("originalTable", height = 700)
  				),
  				tabItem(
  				  tabName = "var_fuzzy",
  					h3("Variable observada a Variable Difusa"),
  					selectInput("variable", "Selección Variable a Fuzzyficar:", choices = NULL),
  					box(width = 12,
  						h3("Diseñar la Variable difusa"),   
  						column( width = 5,
      							   title = "Definir Funciones de membresia",
  						         h5("Elija para ltipo de función de pertenecia"), 
  						        awesomeRadio(
  						          # Identificador del input
  						          inputId = "tipo_set",
  						          # Etiqueta que aparecerá junto a los botones de opción
  						          label = "Radio buttons", 
  						          # Opciones disponibles para el usuario
  						          choices = c("Gaussiana", "Triangular"),
  						          # Disposición de los botones en línea (TRUE para horizontal)
  						          inline = TRUE, 
  						          # Añade un checkbox junto a los botones de opción
  						          checkbox = TRUE
  						        ),
  						        # Título con tamaño de encabezado 5
  						        h5("Indique el dominio de valores"), 
  						        # Input numérico para el valor mínimo
  						        numericInput(
  						          inputId = "min",            # Identificador del input
  						          label = "Mínimo valor: ",   # Etiqueta del input
  						          value = NULL                # Valor inicial (NULL para vacío)
  						        ),
  						        # Input numérico para el valor máximo
  						        numericInput(
  						          inputId = "max",            # Identificador del input
  						          label = "Máximo valor: ",   # Etiqueta del input
  						          value = NULL                # Valor inicial (NULL para vacío)
  						        ),
  						        # Título con tamaño de encabezado 3 (este está vacío para espaciar)
  						        h3(),
  						        # Input de texto para el vector de límites cerrados de rangos
  						        textInput(
  						          inputId = "numeric_vector",                         # Identificador del input
  						          label = "Vector de Límites Cerrados de Rangos (separados por comas)"   # Etiqueta del input
  						        ),
  						        # Título con tamaño de encabezado 5 con ejemplos explicativos
  						        h5("Por ejemplo: si tengo tres intervalos [0a1], (1a3], y (3a5] "),
  						        h5("Los limites cerrados son cuatro: 0, 1, 3 y 5"),
  						        h5("Las etiquetas serán tres: bajo, medio y alto"),
  						        # Input de texto para las etiquetas de los conjuntos
  						        textInput(
  						          inputId = "label_input",      # Identificador del input
  						          label = "Vector Etiquetas de los conjuntos (separadas por comas)"  # Etiqueta del input
  						        ),
  						        # Botón para generar parámetros
  						        actionButton(
  						          inputId = "save",               # Identificador del botón
  						          label = "Generar parámetros"    # Texto del botón
  						        ),
  						        # Tabla de salida para mostrar la descripción de los intervalos
  						        tableOutput("desc_intervalos"),
  						        h3()
  						),
  						# Caja de ancho 6 que contiene una salida de imagen
  						box(
  						  width = 6,  # Ancho de la caja (en una escala de 12 columnas)
  						  
  						  # Salida de imagen con identificador 'funcionM'
  						  imageOutput(
  						    outputId = "funcionM",  # Identificador de la salida de imagen
  						    width = 270,            # Ancho de la imagen en píxeles
  						    height = 120            # Alto de la imagen en píxeles
  						  )
  						),
  						# Otra caja de ancho 6 que contiene una salida de gráfico
  						box(
  						  width = 6,  # Ancho de la caja (en una escala de 12 columnas)
  						  # Salida de gráfico con identificador 'var_plot'
  						  plotOutput(
  						    outputId = 'var_plot',  # Identificador de la salida del gráfico
  						    width = "80%",          # Ancho del gráfico (80% del contenedor)
  						    height = "250px"        # Alto del gráfico en píxeles
  						  )
  						),
  						# Botón para descargar datos
  						downloadButton(
  						  outputId = "downloadData",  # Identificador del botón de descarga
  						  label = "Descargar parámetros de los conjuntos"  # Etiqueta del botón
  						),
  					),
  					box(width = 6,	 
  					    actionButton("fuzzy_btn", "Borrosificar")
  					), 
  					h3(" "),
  					box(
  					  width = 12,  # Ancho de la caja (en una escala de 12 columnas)
  					  h3("Tabla de Datos Borrosificada"), 
  					  # Botón para descargar datos borrosificados en formato Xlsx
  					  downloadButton(
  					    outputId = "download_Xls",  # Identificador del botón de descarga
  					    label = "Descargar datos borrosificada en Xlsx"  # Etiqueta del botón
  					  ),
  					  # Contenedor para la tabla de datos con un spinner de carga
  					  shinycssloaders::withSpinner(
  					    DTOutput("fuzzy_table")  # Salida de la tabla con identificador 'fuzzy_table'
  					  )
  					)
  			)
  )
)

## App completo ----------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",  # Establece el tema de color del dashboard a verde
  header,  # Define el encabezado del dashboard
  sidebar,  # Define la barra lateral del dashboard
  body  # Define el cuerpo del dashboard
)

 server <- function(input, output) {
   
   # DEFINIR FUNCIONES VARIAS
   # Función para calcular la Gaussiana
   gaussiana <- function(x, a, b) {
     # Calcula el valor de la función Gaussiana para un valor x, con parámetros a y b.
     # x: valor para el cual se calcula la Gaussiana
     # a: media de la distribución
     # b: desviación estándar de la distribución
     return(exp(-((x - a) / b)^2))
     # exp(-((x - a) / b)^2) -> la fórmula de la Gaussiana sin la constante de normalización
   }
   # Función para calcular la Triangular
   triangular <- function(x, a, b, c) {
     # Calcula el valor de la función triangular para un valor x, con parámetros a, b y c.
     # x: valor para el cual se calcula la función triangular
     # a: límite inferior del soporte de la función triangular
     # b: modo (pico) de la función triangular
     # c: límite superior del soporte de la función triangular
     return(pmax(
       pmin((x - a) / (b - a), (c - x) / (c - b)), 0))
     # pmin((x - a) / (b - a), (c - x) / (c - b)) -> calcula el valor mínimo entre las dos partes de la función triangular
     # pmax(..., 0) -> asegura que el valor mínimo retornado sea al menos 0
   }
   
   # Inicializa una variable reactiva para almacenar datos como una lista
   datos <- reactiveVal(list())
   # Inicializa una variable reactiva para almacenar factores (inicialmente NULL)
   factores <- reactiveVal(NULL)
   # Inicializa una variable reactiva para almacenar un dataframe (inicialmente NULL)
   df <- reactiveVal(NULL)
   # Inicializa una variable reactiva para almacenar un vector de etiquetas ingresadas por el usuario
   etiquetas <- reactiveVal(c())
   # Inicializa una variable reactiva para almacenar un vector de intervalos
   intervalos <- reactiveVal(c())
   # Inicializa una variable reactiva para almacenar un valor (inicialmente NULL)
   e <- reactiveVal(NULL)
   # Inicializa una variable reactiva para almacenar un historial como una lista
   historial <- reactiveVal(list())
   # Inicializa una variable reactiva para almacenar una tabla de resultados (inicialmente NULL)
   tabla_resultados <- reactiveVal(NULL)
   
   # Cargar datos
   # Observa el evento de carga de un archivo (input$file)
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
   
   # Mostrar la tabla de datos original
   output$originalTable <- DT::renderDataTable({
     # Requiere que se haya cargado un archivo (input$file)
     req(input$file)
     # Combina las columnas cuantitativas y cualitativas almacenadas en 'datos'
     DT::datatable(
       cbind(datos()$cuanti, datos()$cuali),  # Une las columnas cuantitativas y cualitativas
       options = list(scrollX = TRUE)  # Habilita el desplazamiento horizontal
     )
   })
   
   # Mostrar Estadísticas de la tabla numérica
   output$estadi <- renderPrint({
     # Requiere que se haya cargado un archivo (input$file)
     req(input$file)
     # Genera un resumen estadístico de las columnas cuantitativas
     summary(datos()$cuanti)
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
   
   observeEvent(input$save, {
     req(input$numeric_vector) # Asegura que la entrada no sea NULL
     # Divide la cadena en una lista por las comas y convierte a vector numérico
     # Dividir la cadena por las comas y convertir a números
     input_rangos <- as.numeric(unlist(strsplit(input$numeric_vector, ",")))
     # Crear el vector
     numeric_intervals <- c(input_rangos)
     intervalos(numeric_intervals)
     # Dividir y guardar las etiquetas
     etiquetas(strsplit(input$label_input, ",")[[1]])
   })
   
   # Detalle de los rangos
   output$desc_intervalos <- renderTable({
        req(intervalos())
        limites <- intervalos()  #vector con limites cerrados de rangos
        if (length(limites) < 2) {
          stop("No hay suficientes límites para formar intervalos")
        }
        if(input$tipo_set == "Triangular"){
          # Funcion triangular
          tr <- data.frame(
            Minimo = head(limites, -1), # Excluye el último elemento
            Maximo = tail(limites, -1), # Excluye el primer elemento
            b = (limites[-length(limites)] + limites[-1]) / 2,  # Punto central del intervalo
            a = c(limites[1], head((limites[-length(limites)] + limites[-1]) / 2, -1)),  # Desfase de b para obtener a
            c = c(tail((limites[-length(limites)] + limites[-1]) / 2, -1), limites[length(limites)])  # Desfase de b para obtener c
          )
          # Ajustes específicos para el primer y último intervalo
          primero <- 1
          ultimo <- length(limites)-1
          # Ajusto los extremos
          tr$b[primero] <- tr$a[primero]
          tr$b[ultimo] <- tr$c[ultimo]
          tr$etiquetas <-etiquetas() # Añade las etiquetas a la tabla
        }else {
          # Funcion Gaussiana
          tr <- data.frame(
            Minimo = head(limites, -1), # Excluye el último elemento
            Maximo = tail(limites, -1), # Excluye el primer elemento
            # Calcular 'a' como el promedio de límites consecutivos
            a = (head(limites, -1) + tail(limites, -1)) / 2,
            # Calcular 'b' como la mitad de la diferencia entre límites consecutivos
            b = (tail(limites, -1) - head(limites, -1)) / 2
          )
          # Ajustes para los valores extremos de 'a' y 'b'
          primero <- 1
          ultimo <- length(limites)-1
          # Ajuste para el primer y último valor de 'a'
          tr$a[primero]<-tr$Minimo[primero]
          tr$a[ultimo]<-tr$Maximo[ultimo]
          tr$b[primero]<-tr$b[primero]*2
          tr$b[ultimo]<-tr$b[ultimo]*2
          tr$etiquetas <-etiquetas() # Añade las etiquetas a la tabla
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
       datos_intervalos = tabla_resultados()
       # Escribir los datos en un archivo de texto
       sink(file)
       cat("\nDetalles de los Rangos:\n")
       print(datos_intervalos)
       sink()  
     }
   )
    # Grafico variable
    output$var_plot <- renderPlot({
      req(tabla_resultados())
      da <- as.data.frame(cbind(datos()$cuanti, datos()$cuali))
      variable_name <- names(da[input$variable])
      conjuntos <- tabla_resultados()
      gradiente <- nrow(conjuntos)
      min <- input$min
      max <- input$max
      x_vals <- seq(min, max, length.out = 10000)
      titulo  <- switch(input$tipo_set,
                        "Gaussiana" =  "Funciones Gaussianas",
                        "Triangular" = "Funciones Triangulares" 
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
      })
    
    # Borrosificación
    observeEvent(input$fuzzy_btn, {
      # Requiere que la tabla de resultados esté definida
      req(tabla_resultados())
      # Combina las columnas cuantitativas y cualitativas en un dataframe
      da <- as.data.frame(cbind(datos()$cuanti, datos()$cuali))
      # Obtiene el nombre de la variable seleccionada por el usuario
      variable_name <- names(da[input$variable])
      # Obtiene los conjuntos borrosos desde la tabla de resultados
      conjuntos <- tabla_resultados()
      # Calcula el número de gradientes (número de filas en la tabla de conjuntos)
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
      # Asigna las etiquetas a las columnas de los resultados de la inferencia difusa
      colnames(inferencia_resultado) <- etiquetas()
      # Actualiza el historial con los datos borrosificados y la variable original
      historial(list(
        data_fuzzy = inferencia_resultado,  # Datos borrosificados
        var_orig = da[input$variable],      # Variable original
        nombre_variable = variable_name     # Nombre de la variable
      ))
    })
    
      # Visualización de Tabla con números borrosos
      output$fuzzy_table <- DT::renderDataTable({
        req(historial()$data_fuzzy)
        result <- tryCatch(
          expr = {
            # Código que puede generar un error
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
      observeEvent(input$exit_btn, {
        stopApp()  
      })
}

shinyApp(ui, server)
