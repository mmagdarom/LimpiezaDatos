Práctica 2: Limpieza de Datos

Descripción

La presente práctica pertenece a la asignatura de Tipología y Ciclo de Vida de los Datos de la maestría Ciencia de Datos de la UOC. En esta práctica, hemos aplicado técnicas de limpieza y análisis estadistico en el lenguaje R, con el objetivo de identificar los datos relevantes para un proyecto analítico y usar las herramientas de integración, limpieza, validación y análisis de las mismas, al conjunto de datos flavors_of_cacao.csv, que se refiere a la clasificación de barras de chocolate a nivel mundial.

La informacion del DataSet se enfoca principalmente en el chocolate negro simple, con el objetivo de apreciar los sabores del cacao cuando se lo convierte en chocolate. 

Miembros del equipo

  Las integrantes que ejecutamos el desarrollo de la presente práctica somos:

   María Augusta Jimbo Granda
   
   María Magdalena Romero Guzmán

Ficheros del código fuente y DataSet

codigo/chocolateCocoa.R, clase donde se encuentran todos los comandos para realizar la limpieza de datos.

csv/flavors_of_cacao.csv, se encuentra el archivo original del conjunto de datos, se cambió la cabecera, con nombres más cortos para mejorar la comprensión de los mismos.

csv/flavors_of_cacao1.csv, se encuentra el archivo que se reemplazó los elementos vacíos del archivo flavors_of_cacao.csv, que se los representaba con Â por una cadena vacía para realizar un mejor tratamiento de los mismos.

csv/flavors_of_cacao_clean.csv, se encuentra el archivo con los datos limpios.

pdf/LimpiezaCacaoMR_MJ.pdf, se encuentra el archivo con las características del DataSet

Se debe instalar la siguiente libreria:

install.packages("VIM"), para la funcion kNN de los vecinos mas cercanos

Se cargan las siguientes librerias:

library(VIM), para los vecinos mas cercanos

library(car), para las recodificaciones de las variables

library(nortest), para la prueba de normalidad con el método de Anderson-Darling

Recursos:

Covarianza y correlación
- https://www.youtube.com/watch?v=nCnscXRG8Ws   

Homogeneidad de varianza
- https://rpubs.com/Joaquin_AR/218466      

El estadístico de Anderson-Darling
- https://support.minitab.com/es-mx/minitab/18/help-and-how-to/statistics/basic-statistics/supporting-topics/normality/the-anderson-darling-statistic/      

Prueba de Contraste Gráfico
- https://www.youtube.com/watch?v=52Q2gq7tgI0  

Test for homogeneity of variances
- https://biostats.w.uib.no/test-for-homogeneity-of-variances-levenes-test/    

Data Cleaning Basics
- Jason W. Osborne (2010). Data Cleaning Basics: Best Practices in Dealing with
Extreme Scores. Newborn and Infant Nursing Reviews

