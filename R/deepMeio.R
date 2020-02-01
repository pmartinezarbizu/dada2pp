#' Example data deepMeio.RData for dada2pp
#'
#' This dataset is a metabarcoding community table of deep-sea meiofauna collected at 2 localities.
#' The AVSs were blasted against Genbank using a custom script.
#' Each line represent the values of one AVS
#' Columns represent re result of blastn folled by reads per sample
#' Data was generated at Senckenberg am Meer Metabarcoding Laboratory, German Center for Marine Biodiversity Research (DZMB), Wilhelmshaven, Germany
#' 
#' \itemize{
#'   \item ASVno the ASV number
#'   \item pident percentage identity
#'   \item qcovs query coverage             
#'   \item eval e-value
#'   \item length length of query sequence
#'   \item accn accession number of blast hit
#'   \item Group Higher ranking taxonomic group
#'   \item Species Species name of blast hit
#'   \item Columns 9 to 27: location_sample_number 
#'    }
#'@docType data
#'@name deepMeio
#'@keywords datasets data metabarcoding meiofauna deep-sea
#'@usage data(deepMeio)
#'@author Pedro Martinez Arbizu email{pmartinez@@senckenberg.de} 
#'@format A data frame with 500 rows (ASVs), 27 variables(8 blast result, 19 samples)
#'
NULL

