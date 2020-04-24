# Paquete en R para extraer y separar mediciones tomadas con el ceptómetro AccuPAR LP-80
***

## Requisitos
- Instalar rtools para la version de R correspondiente

https://cran.r-project.org/bin/windows/Rtools/history.html

- Instalar el paquete devtools
    
        install.package("devtools")

## Instalación

Copiar y ejecutar el siguiente comando en la terminal de R:

    devtools::install_github("IvanPrzOl/AccuPAR_LP-80_Parsing")

***
## Guía de usuario
El ceptómetro LP-80 puede exportar datos en diferentes formatos de salida como .csv, .txt o .xls  
El formato .xls es un archivo de excel con dos hojas: "Summary Data" y "All Data"

![Ejemplo de archivo](/captures/outputFile_Summary.PNG)

![Ejemplo de archivo](/captures/outputFile_AllData.PNG)

La hoja "Summary Data" muestra los promedios de cada anotación incluida en el archivo.

La hoja "All Data" contiene la información detalla de cada anotación tomada con el ceptómetro y es la hoja que se debe procesar.

### Ejemplo de uso en R
1. Cargar el paquete a la sesión de R con: 
    
        library(AccuPARParsing) 

2. Importar la hoja "All Data" como un dataframe a R studio con el paquete readxl, xlsx o XLConnect.  
    Ejemplo:
        
        library(xslx)
        cept_data <- read.xlsx("cept_file.xls",sheetIndex = "All Data", header = TRUE) 
        
3. Llamar a la función CeptProc:
        
        raw_annotations <- CeptProc(ceptData = cept_data, trialName = 'SYN')
La función acepta los siguientes argumentos:
- ceptData: nombre de la variable del dataframe a procesar.
- trialName: abreviación del ensayo que se desea extraer.
- segments: un vector (1:8) indicando los segmentos a extraer.
- recordOrder: una cadena de texto de la forma 'ABA' para indicar el orden de registro de las anotaciones, ej: 'ABA' para "ABV", "BLW", "ABV".        

4. Dependiendo del valor pasado al parámetro raw (TRUE o FALSE), cept_procesado puede tener los siguientes formatos de salida:

### **raw=TRUE**
![Salida en Raw](/captures/procRaw.PNG "asd")
### **raw = FALSE**
![Salida en means](/captures/procMeans.PNG)
