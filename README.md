# DaFu (Data Fuzzy) Análisis de Datos imprecisos con Lógica borrosa
## Descripción de las Aplicaciones Shiny de Lógica Borrosa
Este documento presenta un conjunto de cuatro aplicaciones desarrolladas en Shiny para R, diseñadas para facilitar la implementación de diversas funcionalidades asociadas a la metodología de lógica borrosa en el análisis de datos. Estas aplicaciones proveen un enfoque interactivo y profundo para la gestión y análisis de conjuntos difusos, adecuado para investigadores y profesionales en áreas como estadística, ciencia de datos, e ingeniería que requieran métodos avanzados para la manipulación de incertidumbres y datos vagos.

## Estructura de las Aplicaciones en el Repositorio
El repositorio DaFu, podría estar estructurado de la siguiente manera:
```
README.md: Un archivo de descripción que ofrece información sobre el proyecto, cómo instalar y ejecutar el código.
00_CrearConjuntosDifusos/
	imagenes/
		ejemplo diagrama Foot.png
		gaussiana.png
		trapezoidal.png
		triangular.png
	app.R
01_Rango_ConjuntosDifusos/
	EJEMPLOS/
	imagenes/
		ejemplo diagrama Foot.png
		gaussiana.png
		trapezoidal.png
		triangular.png
	app.R
02_ParticiónDifusaUnivariada/
	EJEMPLOS/
	app.R
03_AsociacionesDifusas/
	EJEMPLOS/
	app.R
	cluster.carac.R
	dellNULMarg.R
	escalar.R
```

## Clonar el repositorio
Para utilizar el código, primero necesitarás clonar el repositorio en tu máquina local. Esto se puede hacer usando el siguiente comando en la terminal:
git clone https://github.com/matucesari/DaFu.git

## Funcionalidades
Cada directorio dentro del repositorio contiene una aplicación Shiny específica, junto con recursos visuales y ejemplos que ilustran su uso:
### **00_CrearConjuntosDifusos:**
**Funcionalidad**: Esta aplicación Shiny en R está diseñada para permitir a los usuarios trabajar de manera interactiva con conjuntos difusos a través de funciones de membresía específicas, como las gaussianas y triangulares. A continuación, se describen las principales características de esta aplicación:
* **Creación de Conjuntos Difusos**
	- Parámetros Configurables: Los usuarios pueden ingresar de forma dinámica los parámetros necesarios para definir cada conjunto difuso. Esto incluye parámetros como el centro, la amplitud y la desviación estándar para las funciones gaussianas, así como los puntos de inicio, pico y fin para las triangulares.
	- Visualización Individual de Conjuntos: Una vez definidos los parámetros, los usuarios pueden visualizar gráficamente cada conjunto difuso de forma individual. Esto facilita la comprensión inmediata de las características y la forma de cada función de membresía aplicada.
* **Construcción de Variables Lingüísticas Difusas**
	- Combinación de Conjuntos: Los usuarios pueden combinar varios conjuntos difusos para formar una variable lingüística difusa completa.
	- Visualización de Variables Lingüísticas: La variable lingüística completa puede visualizarse a través de una interfaz web externa especializada en graficado de funciones. Esto permite a los usuarios ver cómo los conjuntos individuales se integran para formar la variable completa.
* **Transformación y Análisis de Datos**
	- Selección de Variables de Datos: Los usuarios pueden cargar datos a la aplicación y seleccionar una variable específica para su análisis.
	- Transformación a Tabla de Contingencia: La aplicación transforma la variable seleccionada en una tabla de contingencia que muestra los valores de membresía calculados para cada función de membresía en los conjuntos definidos. Esto es crucial para análisis estadísticos y modelado posterior.
	- Almacenamiento de Datos: La nueva tabla de datos, con los valores de membresía calculados, puede ser guardada para uso futuro. Esto permite una fácil recuperación y uso en estudios continuados o para verificación y comparación de resultados.
* **Interactividad y Usabilidad**
	- Interfaz Amigable: La aplicación cuenta con una interfaz gráfica amigable y fácil de usar, diseñada para facilitar la interacción del usuario con las herramientas analíticas.
	- Integración Web: A través de la integración con herramientas web externas para el graficado, los usuarios pueden obtener una visualización más potente y detallada de las funciones de membresía y variables lingüísticas difusas.

### **01_Rango_ConjuntosDifusos:**
**Funcionalidad**: Las funcionalidades se centran en la personalización de parámetros según los límites de rangos introducidos y las etiquetas especificadas por el usuario. A continuación, se describen las principales características de esta aplicación:
* **Gestión de Variables Cualitativas y Numéricas**
	- Visualización de Datos Originales: Tras cargar los datos, la aplicación muestra la tabla de datos original, permitiendo a los usuarios revisar la información antes de proceder con la borrosificación.
	- Análisis Estadístico y Gráficas: Incluye la generación de estadísticas descriptivas para las variables continuas y una gráfica de matriz de correlación. Esta última puede colorearse en función de una variable categórica, proporcionando una referencia visual útil sobre las relaciones entre variables.
	- Referencias de Variables Nominales: Ofrece descripciones y referencias sobre las variables nominales incluidas en los datos, facilitando la interpretación y manejo de estas categorías.
* **Exploración de Rangos y Estimación de Parámetros**
	- Configuración de Rangos Booleanos: Los usuarios pueden introducir los límites de rangos booleanos para cada conjunto difuso deseado. Esto facilita la delimitación de áreas específicas dentro de una distribución de datos o una escala de medición.
	- Estimación Automática de Parámetros: Basándose en los rangos y etiquetas proporcionadas, la aplicación estima automáticamente los parámetros necesarios para las funciones de membresía de cada conjunto. Esto incluye determinar centros, anchos y formas óptimas para funciones como gaussianas o triangulares.
* **Creación y Visualización de Variables Lingüísticas Difusas**
	- Construcción de Variables Lingüísticas: Los conjuntos difusos definidos pueden ser combinados para formar una variable lingüística completa, representando un espectro más amplio de características dentro del conjunto de datos.
	- Visualización Integrada: La variable lingüística construida puede ser visualizada directamente en la interfaz de la aplicación, mostrando la interacción y superposición de los conjuntos individuales, lo que permite una evaluación instantánea de la configuración completa.
* **Manipulación y Análisis de Datos**
	- Selección de Variables de Datos Cargados: Los usuarios pueden cargar conjuntos de datos a la aplicación y seleccionar variables específicas para análisis.
	- Transformación a Tabla de Contingencia: La aplicación convierte la variable seleccionada en una tabla de contingencia que refleja los valores de membresía calculados para cada función de membresía en los conjuntos. Esta función es fundamental para aplicaciones en análisis estadístico y modelado de datos.
	- Guardado de Datos: Es posible guardar la nueva tabla de datos generada, facilitando así el acceso futuro para comparaciones, verificaciones o estudios continuados.
* **Interfaz y Usabilidad**
	- Interfaz Usuario-Friendly: Diseñada para ser intuitiva y fácil de usar, la aplicación permite a los usuarios sin experiencia previa en lógica difusa interactuar eficazmente con las herramientas analíticas.
	- Exportación de Resultados: Los usuarios pueden exportar los resultados y configuraciones de conjuntos difusos para su uso en otros software o para la documentación y reporte de investigación.

### **02_ParticiónDifusaUnivariada:**
**Funcionalidad**: Esta aplicación Shiny en R está diseñada para simplificar y automatizar la creación de conjuntos difusos aplicando un método avanzado de partición univariada de clases óptima. Está orientada a facilitar la discretización y el análisis de variables tanto numéricas como cualitativas, y la construcción de variables lingüísticas difusas de manera visual e intuitiva. Las principales características de la aplicación se detallan a continuación:
* **Gestión de Variables Cualitativas y Numéricas**
	- Visualización de Datos Originales: Tras cargar los datos, la aplicación muestra la tabla de datos original, permitiendo a los usuarios revisar la información antes de proceder con la borrosificación.
	- Análisis Estadístico y Gráficas: Incluye la generación de estadísticas descriptivas para las variables continuas y una gráfica de matriz de correlación. Esta última puede colorearse en función de una variable categórica, proporcionando una referencia visual útil sobre las relaciones entre variables.
	- Referencias de Variables Nominales: Ofrece descripciones y referencias sobre las variables nominales incluidas en los datos, facilitando la interpretación y manejo de estas categorías.
	- Codificación de Variables Cualitativas: Las variables cualitativas son transformadas a una forma discreta para facilitar su discretización junto con las variables numéricas.
	- Discretización Personalizada: La aplicación ajusta la discretización a la variabilidad específica de la variable, ya sea numérica o codificada de forma cualitativa, proporcionando un tratamiento unificado y sistemático.
* **Creación de Rangos y Estimación de Parámetros**
	- Algoritmo de Partición Univariada de Clases Óptima: La aplicación utiliza un algoritmo sofisticado para dividir una variable seleccionada en clases óptimas, maximizando la homogeneidad dentro de las clases y la heterogeneidad entre ellas. Esto es especialmente útil para identificar de manera efectiva los límites de rangos en conjuntos de datos complejos.
	- Estimación Automática de Parámetros: Una vez definidos los rangos, el sistema estima los parámetros necesarios para las funciones de membresía de cada conjunto difuso, tales como los centros y las amplitudes, a partir de los límites de rangos y etiquetas asignadas por el usuario.
* **Construcción y Visualización de Variables Lingüísticas Difusas**
	- Construcción de Variables Lingüísticas Difusas: Los conjuntos difusos creados pueden ser integrados para formar una variable lingüística difusa, representando un espectro más completo de la data.
	- Visualización en la Interfaz: La interfaz permite a los usuarios visualizar la variable lingüística difusa completa, mostrando claramente cómo los diferentes conjuntos se superponen y contribuyen al modelo general.
* **Análisis y Almacenamiento de Datos**
	- Transformación de Variables en Tablas de Contingencia: Después de seleccionar una variable de un conjunto de datos cargado, la aplicación transforma esta variable en una tabla de contingencia que muestra los valores de membresía de cada función de membresía en los conjuntos definidos.
	- Guardado de Tablas de Datos: La tabla de contingencia generada, que incluye los valores de membresía, puede ser guardada para futuras referencias o análisis, facilitando la gestión y revisión de los datos.
* **Facilidad de Uso y Exportación**
	- Interfaz Intuitiva: Con un diseño enfocado en la facilidad de uso, la aplicación permite a usuarios de todos los niveles manejar complejas configuraciones de lógica difusa sin necesidad de conocimientos técnicos profundos.
	- Exportación de Resultados: Los resultados y configuraciones pueden ser exportados para su uso en otros análisis, documentación o presentaciones.

### **03_AsociacionesDifusas:**
Funcionalidad: Esta aplicación de Shiny es la cuarta de una serie diseñada para implementar y visualizar distintas funcionalidades asociadas al manejo y análisis de tablas de contingencia. En este caso, se enfoca en la toma de una tabla de contingencia derivada de las otras aplicaciones, que contiene los valores de membresía de cada conjunto perteneciente a una variable difusa.
Funcionalidades Principales:
* **Agrupación y Suma Algebraica**: Esta función agrupa filas basadas en criterios seleccionados por combinación de otras variables categóricas. Aplica la suma algebraica para realizar una agregación difusa, combinando los grados de pertenencia de las filas agrupadas en un valor colectivo, lo que ofrece una nueva perspectiva sobre los datos.
* **Extracción de Asociaciones**: Analiza la tabla de contingencia para identificar asociaciones significativas entre las filas y columnas. Utiliza pruebas de asociación basadas en la prueba exacta de Fisher, permitiendo detectar relaciones estadísticamente significativas. 
* **Visualización de Datos**: Emplea el método del Análisis de Correspondencia (CA) para explorar y visualizar las relaciones entre filas y columnas en un plano factorial. Esta técnica facilita la interpretación y extracción de conclusiones visualmente intuitivas.
* **Guardar Tablas Escaladas**: Permite guardar la tabla de contingencia original escalada y la tabla de contingencia agregada escalada localmente.
* **Exportación de Datos**: Los usuarios pueden descargar la tabla agregada en formato CSV, así como los resultados de las pruebas de asociación y los resultados del Análisis de Correspondencia, incluyendo gráficos y tablas

## Utilización de la aplicación en Shiny
Para utilizar esta aplicación Shiny, aquí tienes los pasos básicos que cualquier usuario debería seguir, y que podrías incluir en tu tesis:
1. **Instalación y ejecución:**
* Asegurarse de que R y RStudio están instalados.
* Clonar o descargar el repositorio desde GitHub.
* Abrir el archivo ```app.R``` en RStudio.
* Instalar las dependencias necesarias como se muestra al inicio del script.
* Ejecutar la aplicación haciendo uso de ```shiny::runApp()``` en el mismo directorio que el archivo ```app.R```.
2. **Interacción con la aplicación:**
* Los usuarios pueden interactuar a través de la barra lateral y los paneles de entrada de datos.
* Las predicciones o cálculos se realizan utilizando los botones y controles disponibles en la interfaz.

## Cómo Contribuir
Invitamos a la comunidad a contribuir al desarrollo y mejora de estas herramientas. Los interesados pueden clonar el repositorio, realizar modificaciones y proponer cambios a través de pull requests.


## Notas Generales
Asegúrate de tener la última versión de R y RStudio.
Verifica que todas las bibliotecas necesarias estén instaladas.


