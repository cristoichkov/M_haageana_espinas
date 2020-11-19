# Medición y análisis de espinas centrales y radiales en el complejo *Mammillaria haageana*

- Pequeña introducción de por que se realizo este repo.


##### Para mas informacion sobre espinas en Cactáceas y el método de medición revisar la seccion de [Wiki]("poner liga a wiki")  

#### Prerequisites

##### Software:
- [ImageJ 1.52a](https://imagej.nih.gov/ij/)
- [R 3.6.3](https://www.r-project.org/)
- [RStudio 3.6.3](https://rstudio.com/)


##### R packages:
- [tidyr- 1.1.2](https://tidyr.tidyverse.org/)
- [dplyr - 1.0.2](https://dplyr.tidyverse.org/)
- [ggplot2 - 1.2.1335](https://ggplot2.tidyverse.org/)
- NISTunits - 1.0.1
- cluster - 2.1.0
- htmltools - 0.4.0
- webshot - 0.5.2

#### Directories:
###### bin
Contains:
  * R function `.R`
    * `read_esp_cen.R`.- extracts the tables of stats.txt file of ipyrad output folder.
    * `read_esp_rad.R`.- calculates the bootstrap mean of RAxML_bipartitionsBranchLabels tree.
    * `form_are_esp_rad.R`.- calculates the bootstrap mean of RAxML_bipartitionsBranchLabels tree.
    * `export_formattable.R`.- calculates the bootstrap mean of RAxML_bipartitionsBranchLabels tree.


  * R script `.r`
    *  `BD_espinas_rad_cen.r`.-  works to calculate the percentage of match reads and no match reads of the demultiplex process used stacks, ipyrad and gbsx.
    *  `BD_PCA.r`.-  works to calculate the percentage of match reads and no match reads of the demultiplex process used stacks, ipyrad and gbsx.


* note.- the folder `unused` contains the scripts that I use to practice and probe the final scripts


###### data

Contains the data demultiplex with GBSX:
 * `Mesures`.-

###### meta
Contains information about the samples:

###### out
Contains the results of all analysis:


#### Notes


#### Credits
##### Beatríz Gil Peña y [Cristian Cervantes](https://cristoichkov.github.io/)
