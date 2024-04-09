#' Loading the Respiration Signal
#'
#' Function to load photoplethysmographic (PPG) signal.
#'
#' @param file_name A .csv file with PPG signal
#' @param path Signal sampling rate.
#' @param first_sample Define the first sample to load
#' @param last_sample Define the last sample to load
#'
#' @return A numeric vector contains the PPG signal
#'
#' @export

loadPPG <- function(file_name, path = './', first_sample = 0, last_sample = 1000){
  # input checking
  stopifnot(is.numeric(first_sample));
  stopifnot(is.numeric(last_sample));
  if (first_sample%%1!=0){
    stop('Error: first_sample must be integer!')
  }
  if (first_sample<0){
    stop('Error: first_sample must be positive!')
  }
  if (last_sample%%1!=0){
    stop('Error: first_sample must be integer!')
  }
  if (last_sample<first_sample){
    stop('Error: last_sample must be grater than first_sample!')
  }
  stopifnot(is.character(file_name))
  stopifnot(is.character(path))
  if (stringi::stri_sub(file_name,-4,-1) != '.csv'){
    stop('Error: file_name must be .csv!')
  }

  #path+file_name
  file_path <-paste0(path, file_name)

  # loading example data
  signals <- utils::read.csv(file = file_path, header = FALSE);
  raw_ppg = signals$V1;

  if (last_sample>length(raw_ppg)){
    stop('Error: last_sample is over signal shape!')
  }
  raw_ppg = raw_ppg[first_sample:last_sample];

  #plot loaded data
  plot(raw_ppg, type='l')

  return(raw_ppg)
  }

