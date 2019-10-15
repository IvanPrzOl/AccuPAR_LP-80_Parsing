# R package to parse and order data from the AccuPAR LP-80 output files.
***

## Instalación
Antes de instalar este paquete hay que instalar la herramienta devtools ejecutanto el siguiente comando en R studio

    install.library("devtools")

Instalar el paquete para procesar datos del ceptómetro con:
    
    devtools::install_github("IvanPrzOl/AccuPAR_LP-80_Parsing")

***
## Descripción del archivo de salida
El ceptómetro LP-80 puede exportar datos en diferentes formatos de salida como .csv, .txt o .xls  
El formato .xls es un archivo de excel dos hojas:

![Ejemplo de archivo](/captures/outputFile_Summary.PNG)

![Ejemplo de archivo](/captures/outputFile_AllData.PNG)

La hoja "Summary Data" contiene los promedios de cada anotación guardada en el ceptómetro.

La hoja "All Data" contiene la información detalla de cada anotación que se guardó en el ceptómetro y es la hoja que nos interesa procesar.

## Uso
1. Cargar el paquete a la sesión de R con: 
    
        library(AccuPARParsing) 

2. Importar la hoja "All Data" como un dataframe a R studio con el paquete readxl, xlsx o XLConnect.  
    Ejemplo:
        
        library(xslx)
        cept_data <- read.xlsx("cept_file.xls",sheetIndex = 2, header = TRUE) 
        
3. Llamar a la función SubsetAnn para procesar los datos del ceptometro indicando los siguientes parámetros.
        
        # df, dataframe importado del archivo excel.
        # tName, Iniciales del ensayo que se desea extraer.
        # nRecords, Número de muestras tomadas por cada anotación que corresponden a cada uno de los estratos medidos.
        # segments, El número de los segmentos a extraer.
        # raw, Indica si la salida de la función contiene los promedios de cada estrato o los datos crudos.
        cept_procesado <- SubsetAnn( df = cept_data,
                                     tName = "HIB", 
                                     nRecords =3, 
                                     segments = 1:8,
                                     raw = TRUE)

4. Dependiendo del valor pasado al parámetro raw (TRUE o FALSE), cept_procesado puede tener los siguientes formatos:
 

