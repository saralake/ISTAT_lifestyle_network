rm(list=ls())

import_folder = 'Original_data/'
export_folder = 'Intermediate_data/'

if(!dir.exists(export_folder)){
  dir.create(export_folder)
}

all_dat = read.table(paste0(import_folder, 'AVQ_Microdati_2023.txt'), header = TRUE, sep = "\t")
colnames(all_dat) # la legenda si trova qui file:///Users/saralago/Downloads/AVQ_2023_IT/METADATI/AVQ_Tracciato_2023.html

