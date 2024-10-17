# Librerías
libraries <- c("shiny", "FuzzyR", "shinyWidgets", "shinydashboard", "DataExplorer","tidyr",
  "openxlsx", "dplyr", "xray", "DT","htmlwidgets")

# Instala los paquetes si no están instalados
install.packages(setdiff(libraries, rownames(installed.packages())), dependencies = TRUE)

# Cargar librerías
lapply(libraries, library, character.only = TRUE)

# Definir una Funcion Graficar Conjunto Difuso
graficar <- function(x, mf, tipo){
  # Comprobar si 'mf' (conjunto de datos) no es NULL
  if (!is.null(mf)){
    # Si 'mf' existe, se procede a graficar  
    # Utilizar la función 'plot' para crear la gráfica
    plot(x,            # Eje X: Datos de entrada 'x'
         mf,           # Eje Y: Grado de pertenencia 'mf'
         type = "l",   # Tipo de gráfica: línea ('l')
         xlab = "x",   # Etiqueta del eje X: 'x'
         ylab = "Grado de pertenencia", # Etiqueta del eje Y
         col = "purple", # Color de la línea: púrpura
         lwd=2,        # Ancho de la línea: 2
         main = paste("Función de pertenencia ", tipo) # Título de la gráfica, combinando "Función" con el valor de 'tipo'
    )
    # Esta gráfica mostrará cómo varía el grado de pertenencia de 'mf' con respecto a 'x',
    # lo cual es útil para visualizar funciones de pertenencia en lógica difusa, por ejemplo.
  }
  # Si 'mf' es NULL, la función no realiza ninguna acción/gráfica.
}

# Define la interfaz de usuario de Shiny
  # Encabezado --------------------------------------------------------------
  header <- dashboardHeader( title="Definir conjuntos difusos y aplicar la función de pertenencia" )

  # Crear la barra lateral (sidebar) de un dashboard------------------------
   sidebar <- dashboardSidebar(
    # Incluir un menú en la barra lateral
    sidebarMenu(
      # Añadir un ítem al menú para la tabla de datos original
      menuItem("Tabla de Datos Original", 
               tabName = "datos_ori", 
               icon = icon("table")),
      # Añadir un ítem para ajustes relacionados con la variable difusa
      menuItem("Diseño Variable Difusa", 
               tabName = "var_fuzzy", 
               icon = icon("cog", lib = "glyphicon")),
      # Añadir un ítem para la funcionalidad de borrosificación
	    menuItem("Borrosificación", 
               tabName = "borro", 
               icon = icon("list-alt"))
    )
    # Incluir un botón de acción para 'Salir' de alguna funcionalidad o de la app
    # actionButton("exit_btn", "Salir")    
  )
  
  # Cuerpo principal de la aplicación Shiny------------------------
  body <- dashboardBody(
    # Definir los elementos tabulares para diferentes secciones de la UI
  	tabItems(
        # Primera pestaña: Carga y visualización de datos originales
  		tabItem(
  			tabName = "datos_ori",
  			h2("Carga de fichero CSV con tabla de Datos"), 
  			box(width = 12,
  				      # Instrucciones para el usuario
  				      h5("La tabla CSV DEBE tener en 1er columna: etiqueta de observaciones (ID único de fila)"), 
  				      # Interruptores para especificar características del archivo CSV
  				      materialSwitch(inputId = "header", "El archivo TIENE Encabezados de columnas", value = TRUE),
  				      materialSwitch(inputId = "nominales", "La tabla CONTIENE Variables Categóricas Nominales", value = FALSE),
  				      # Opciones para especificar el formato del archivo CSV
  				      radioButtons("sep", "Separador de columnas:", choices = c(Coma = ",", Punto_y_Coma = ";", Tabulador = "\t"), selected = ";"),
  				      radioButtons("dec", "Separador decimal:", choices = c(Coma = ",", Punto = "."), selected = '.'),
  				      fileInput("file", "Selecciona un fichero CSV:", accept = ".csv")
  			),
  			h2("Datos Originales"),
  			# Visualización de estadísticas y datos originales
  			 box(width = 12,
  				h3("Estadísticas de las Variables Cuantitativas"),
  				verbatimTextOutput("estadi")
  			 ),
  			h3("Tabla de Datos"),
  			DTOutput("originalTable", height = 700)
  		),
        # Segunda pestaña: Diseño y configuración de variables difusas
  		tabItem(
  			 tabName = "var_fuzzy",
         # Configuración para la selección y borrosificación de variables
  			 h3("Variable Original a Variable Difusa"),
  			 selectInput("variable", "Selección Variable a Borrosificar:", choices = NULL),
  			 actionButton("botonFuzzyficar", "Fuzzyficar"),
         # Contenedor para el diseño de la variable difusa
  			 box(
  				width = 12,
  				column(
      				   width = 5, 
      				   title = "Definir Función Pertenencia",
      				   h5("Elija, para un conjunto difuso, tipo de función de pertenecia"), 
      				   awesomeRadio(
      				        	inputId = "tipo_set",
      				        	label = "Radio buttons", 
      				        	choices = c("Gaussiana", "Triangular"),
      				        	inline = TRUE, 
      				        	checkbox = TRUE
      			 	   ),
      				    h5("Seleccione los paámetros para un conjunto"), 
      			    	numericInput("min", "Dominio mínimo de valores: ",NULL),
      			    	numericInput("max", "Dominio máximo de valores: ",NULL),
      			    	h4("Parámetros"),
      			    	conditionalPanel(
      				        condition = "input.tipo_set == 'Gaussiana'",
      				        sliderInput("sga", "a: ",min = 0, max =5, value =0, step = 1),
      				        numericInput("ga", "valor", value =NULL),
      				        numericInput("gb", "b: ",value =NULL),
      				        h5("gaussmf tiene como parametro a y b, que son los parámetros para media y desvio de la campana de gauss")
      			      	),
      			    	conditionalPanel(
      				        condition = "input.tipo_set == 'Triangular'",
      				        sliderInput("stria", "a: ",min = 0, max =5, value =0, step = 1),
      				        numericInput("tria", "valor",value =NULL),
      				        sliderInput("strib", "b: ",min = 0, max =5, value =0, step = 1),
      				        numericInput("trib", "valor",value =NULL),
      				        sliderInput("stric", "c: ",min = 0, max =5, value =0, step = 1),
      				        numericInput("tric", "valor",value =NULL),
      				        h5("tri_mf presenta parámetros (a, b, c), donde a y c localiza los pies del triángulo y b localiza el pico")
      			      ),
      			  	h3(),
      			  	h5("Antes de añadir un conjunto coloque una etiqueta lingüística"), 
      			  	textInput("nombre_mf", "Etiqueta conjunto difuso para añadir a la VD: "),
      			  	h5("Fórmula para graficar con app abajo: "),
      			  	verbatimTextOutput("vf"), 
      			  	h3()
  			    ),
  				box(
  				    width = 6, 
  				    imageOutput(outputId ="funcionM", width = 270, height = 120)
  				),
      		conditionalPanel(
        				  condition = "input.gb > 0 || input.tric > 0",
        				  box( width = 7, 
          				       plotOutput( outputId = 'plot',width = "90%",height = "250px"),
          				       h4(),
          				       h5("Con el graficador de abajo, puede ver en simultanea todos los conjuntos"), 
          				       h5("ANTES de AÑADIR, revise todos los conjuntos y los agrega uno por uno"), 
          				       h5("No es posible borrar conjuntos agregados, debera comenzar de nuevo")
        				  ),
        				  box( width = 7, 
        				       # Botón para añadir el conjunto difuso
        				       actionButton("btn_anadir", "Añadir Conjunto"),
        				       h5("Acepte la Notificación")
        				  )
      		),
          # Herramientas adicionales para gráficas de funciones de pertenencia
          	box(width = 12,
                tags$iframe(src = "https://pfortuny.net/fooplot.com/", width = "100%", height = "400px")
          )
  		  )
  		),
      # Tercera pestaña: Visualización de la tabla de datos borrosificada
  		tabItem(
  		    tabName ="borro",
  			  box(
  			    width = 12,  # Ancho de la caja (en una escala de 12 columnas)
  			    h3("Tabla de Datos Borrosificada"), 
  			    # Botón para descargar datos borrosificados en formato Xlsx
  			    downloadButton(
  			      outputId = "download_Xls",  # Identificador del botón de descarga
  			      label = "Descargar datos borrosificada en XLSx"  # Etiqueta del botón
  			    ),
  			    # Contenedor para la tabla de datos con un spinner de carga  			    
  			      DTOutput("fuzzy_table")  # Salida de la tabla con identificador 'fuzzy_table'
  			 )
  		)
  	)
 )


# Definir la interfaz de usuario completa de la aplicación-------------------------
ui <- dashboardPage(
    skin = "green",  # Aplicar un tema de color verde a la interfaz
    header,  # Incluir el encabezado definido previamente
    sidebar, # Incluir la barra lateral definida previamente
    body     # Incluir el cuerpo principal definido previamente
)     

# Definición del servidor en una aplicación Shiny---------------------------------
 server <- function(input, output) {
      # Inicialización de valores reactivos para almacenar datos, factores, y otros elementos necesarios para la aplicación.
      datos <-  reactiveVal(list())
      factores <- reactiveVal(NULL)
      df <- reactiveVal(NULL)
      
      # Inicialización de valores para el manejo de conjuntos difusos.
      pmf <- reactiveVal(NULL)
      contador <- reactiveVal(0)
      mensaje <- reactiveVal(NULL)
    
      # Almacenamiento de historial y preparación de variables temporales y etiquetas.
      historial <- reactiveVal(list())
      temp <- reactiveVal(list(c()))
      conjuntos <- c()
      etiquetas <- c()
      
     # Observa eventos relacionados con cambios en las entradas del dominio (máximo y mínimo) y actualiza los controles de la UI correspondientemente.
     # Validar entradas  de Dominio
      observeEvent(input$max, {
        updateSliderInput(getDefaultReactiveDomain(), "sga", max = input$max)
        updateSliderInput(getDefaultReactiveDomain(), "stria", max = input$max)
        updateSliderInput(getDefaultReactiveDomain(), "strib", max = input$max)
        updateSliderInput(getDefaultReactiveDomain(), "stric", max = input$max)
        # Realizar la validación aquí
        if (!is.na(input$min) && !is.na(input$max)) {
           if (input$min > input$max) {
              # Acciones en caso de que la validación falle
              # Puedes mostrar un mensaje de error, revertir el cambio, etc.
              showModal(modalDialog(
                title = "Error de Validación de Dominio",
                "El valor mínimo debe ser menor que el maximo",
                easyClose = TRUE
              ))
              # También puedes revertir el cambio
              updateNumericInput(getDefaultReactiveDomain(), "max", value = 0)
           } 
        }
       })
      observeEvent(input$min, {
        updateSliderInput(getDefaultReactiveDomain(), "sga", min = input$min)
        updateSliderInput(getDefaultReactiveDomain(), "stria", min = input$min)
        updateSliderInput(getDefaultReactiveDomain(), "strib", min = input$min)
        updateSliderInput(getDefaultReactiveDomain(), "stric", min = input$min)        
        if (!is.na(input$min) && !is.na(input$max) ){
           if(input$min > input$max){
              # Acciones en caso de que la validación falle
              showModal(modalDialog(title = "Error de Validación de Dominio",
                "El valor mínimo debe ser menor que el maximo",
                easyClose = TRUE
              ))
              updateNumericInput(getDefaultReactiveDomain(), "min", value = 0)
           } 
        }
      })
      
      #Agrego una entrada numerica al Slider
      observeEvent(input$sga, {
        updateNumericInput(getDefaultReactiveDomain(), "ga", value = input$sga)
      })
      observeEvent(input$ga, {
        updateSliderInput(getDefaultReactiveDomain(), "sga", value = input$ga)
      })
      observeEvent(input$stria, {
        updateNumericInput(getDefaultReactiveDomain(), "tria", value = input$stria)
      })
      observeEvent(input$tria, {
        updateSliderInput(getDefaultReactiveDomain(), "stria", value = input$tria)
      })
      observeEvent(input$strib, {
        updateNumericInput(getDefaultReactiveDomain(), "trib", value = input$strib)
      })
      observeEvent(input$trib, {
        updateSliderInput(getDefaultReactiveDomain(), "strib", value = input$trib)
      })
      observeEvent(input$stric, {
        updateNumericInput(getDefaultReactiveDomain(), "tric", value = input$stric)
      })
      observeEvent(input$tric, {
        updateSliderInput(getDefaultReactiveDomain(), "stric", value = input$tric)
      })
      
     # Manejo de la carga de archivos CSV, incluyendo configuraciones de lectura y preprocesamiento inicial.
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
          df(),  # Une las columnas cuantitativas y cualitativas
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

      # Muestra la imagen correspondiente a la función seleccionada.
      output$funcionM <- renderImage({
        switch(input$tipo_set,
               "Gaussiana" =  ruta_relativa <- file.path("imagenes", "gaussiana.png"),
               "Triangular" = ruta_relativa <- file.path("imagenes", "triangular.png") 
        )
        # Devolver la información de la imagen
        list(src = ruta_relativa,
             contentType = "png",
             width = 250,
             height = 120)
      }, deleteFile = FALSE)  # Establecer delete File a FALSE para no eliminar la imagen temporalmente
      
      # Muestra las fórmulas de las funciones seleccionadas (gaussiana o triangular).      
      output$formulaGauss <- renderPrint({
       	# Mostrar formula para graficar función gaussiana
        HTML(sprintf(" e^-(((x-%s)/%s)^2)",
                            ("a"), ("b")  )  )
      })
      output$formulaTri <- renderPrint({
        # Mostrar formula para graficar función triangular
	      HTML(sprintf(" max(min((x-%s)/(%s-%s),(%s-x)/(%s-%s)),0)", 
                     ("a"), ("b"), ("a"),  ("c"),  ("c"),  ("b") ) )
      })
      
      # Renderiza un gráfico personalizado según la función y parámetros seleccionados.
      output$plot <- renderPlot({
        req(input$max)
        x <- seq(input$min, input$max, length.out = 1000)
        result <- tryCatch(
          expr = {
            # Grafico individual
            if (input$tipo_set=="Gaussiana" ){
              if ( (!is.na(input$ga) && !is.na(input$gb)) ) 
              {
                pmf(dnorm(x, mean = input$ga, sd = input$gb))
                graficar(x,pmf(),input$tipo_set)
              }
            }
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Error en Graficado de función Gaussiana",
              paste("Se ha producido un error al intentar calcular y graficar la Función gaussiana:", conditionMessage(e), "\n"),
              easyClose = TRUE
            ))
            pmf <- NULL
          }
        )
        result <- tryCatch(
          expr = {
            # Código que puede generar un error
            # Dibujo
            if (input$tipo_set=="Triangular" ){
              if ( (!is.null(input$tria) && !is.null(input$trib) && !is.null(input$tric))) 
              {
                pmf(pmax(
                  pmin(
                    (x-input$tria)/(input$trib-input$tria),
                    (input$tric-x)/(input$tric-input$trib)
                  ), 
                  0) )
                graficar(x,pmf(),input$tipo_set)
              }  
            }
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Error en Graficado de función Triangular",
              paste("Se ha producido un error al intentar calcular y graficar la Función Triangular:", conditionMessage(e), "\n"),
              easyClose = TRUE
            ))
            pmf <- NULL
          }
        )
      })
      
	# Maneja la adición de un nuevo conjunto difuso según los parámetros ingresados por el usuario.      
      observeEvent(input$btn_anadir, {
        req(input$nombre_mf)
        # Botón Añadir
        result <- tryCatch(
          expr = {
            # Crear el conjunto difuso según los parámetros ingresados
            mf<-switch(input$tipo_set,
                       "Gaussiana" = genmf('gaussmf', c(input$gb,input$ga)),
                       "Triangular" = genmf('trimf', c(input$tria, input$trib, input$tric))
            )            
            # Agrega el nuevo elemento a la lista reactiva
            conjuntos <- cbind(temp()$c, mf)            
            e <- sapply(input$nombre_mf, function(name) {
              name <- gsub("[[:space:].(){}\\[\\]]", "", name)  # Eliminar caracteres no deseados
              make.names(name, unique = TRUE)  # Convertir en nombre válido y único
            })
            etiquetas <- c(temp()$e, e)
            temp(list(c=conjuntos,e=etiquetas))
          
            # Puedes incrementar un contador para contar los conjuntos añadidos
            contador(contador() + 1)
            mensaje(paste(contador(),"-Se añadio satisfactoriamente el conjunto ",input$nombre_mf) )
            # Notifico
            showModal(modalDialog(
              title = "Diseño de Variable Difusa",
              mensaje(),
              easyClose = FALSE
            ))
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Error en Diseño de la Variable Difusa",
              paste("Se ha producido un error en agregar conjunto mf ", conditionMessage(e), "\n"),
              easyClose = TRUE
            ))
          }
        )
      })
      
      #Muestro la formula del conjunto para hacer diagrama de variable
      output$vf <- renderPrint({
        req(input$nombre_mf)
        # Obtengo la función con sus parámetros
        switch(input$tipo_set,
               "Gaussiana" = HTML(sprintf("e^(-((x-%s)/%s)^2)", input$ga,input$gb )),
               "Triangular" =  HTML(sprintf("max(min((x-%s)/(%s-%s),(%s-x)/(%s-%s)),0)",
                                            input$tria,input$trib,input$tria, input$tric,input$tric, input$trib))
        )
      })

      # Borrosifica la variable seleccionada 
      observeEvent(input$botonFuzzyficar, {
        req(input$variable, historial())    # Botón Borrosificar
        # Borrosificar la variable seleccionada
        da <- as.data.frame(cbind(datos()$cuanti,datos()$cuali))
        print(da)
        variable_name <- names(da[input$variable])
         result <- tryCatch(
          expr = {
            # Aplicamos la función de pertenencia de los conjuntos difusos
            inferencia <- evalmf(da[[variable_name]], temp()$c)
            resultados <- as.data.frame(inferencia)
            colnames(resultados) <-  temp()$e
            historial(list(
              data_fuzzy = resultados, 
              var_orig = da[input$variable],
              nombre_variable = variable_name))
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Error en Borrosificación",
              paste("Se ha producido un error en aplicar función ", e$message),
              easyClose = TRUE
            ))
          }
        )

      # muestra los resultados en una tabla.
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
    })
      
      # Permite descargar los resultados de la borrosificación en un archivo CSV.  
      output$download_Xls <- downloadHandler(
        filename = function() {
          paste(historial()$nombre_variable, "_Fuzzy.csv")
        },
        content = function(file) {
          f <-as.data.frame.matrix(historial()$data_fuzzy)
          if(!is.null(factores())){
            r <- data.frame(f,as.data.frame(factores()))
            print(r)
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
            
     # Detiene la aplicación Shiny cuando se presiona el botón de salida.
      #  observeEvent(input$exit_btn, {
      #    stopApp()  
      #  })  
}

shinyApp(ui, server)
