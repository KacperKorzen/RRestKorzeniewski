#' Estimating the Respiration Signal
#'
#' Function estimating the respiratory signal from a photoplethysmographic (PPG)
#' curve using FM modulation
#'
#' @param raw_ppg Vector giving the samples of PPG signal.
#' @param sample_rate Signal sampling rate.
#' @param normalise_resp Specifies if the normalization to be used.
#
#' @return A S3 class "Signal" object which containing information about a signal.
#'         It has four key attributes: "signal", "time", "signal_type", and "graph".
#'         The "signal" attribute holds the actual signal value, while "time" denotes the time.
#'         "signal_type" identifies the type of the signal. Lastly, the "graph" attribute contains
#'         a graphical representation of the signal.
#'
#'  @examples
#'           ppgToResp(raw_ppg = ppg_signal, sample_rate = 500);
#'           ppgToResp(raw_ppg = ppg_signal, sample_rate = 500, normalise_resp = FALSE);
#'
#' @export ppgToResp
ppgToResp <- function(raw_ppg, sample_rate, normalise_resp = TRUE){
  # input checking
  stopifnot(is.numeric(raw_ppg))
  stopifnot(is.numeric(sample_rate))
  stopifnot(is.logical(normalise_resp))

  if(length(raw_ppg)==0){
    stop("Error: Vector of raw signal can't be empty!");
  }

  # calculate vector of time
  if(sample_rate < 0){
    stop("Error: Sampline rate must be positive value!");
  }
  n_samples = length(raw_ppg);
  time_step = n_samples/sample_rate;
  time = pracma::linspace(0, time_step, n_samples);

  # get max values and location
  peaks <- gsignal::findpeaks(c(raw_ppg), DoubleSided = TRUE, MinPeakHeight = 0.5)
  if(length(peaks$pks) == 0){
    stop("Error: There were not any extremum in singal!");
  }

  # get time steps where maximum appear
  extremes_time = time[peaks$loc];
  if (length(extremes_time) < 2)
    stop('Error: There was neded at least two extrem point!');

  # calculate resp signal from FM modulation
  resp = diff(extremes_time);
  if (normalise_resp == TRUE){
    resp = (resp-min(resp))/max(resp)-min(resp);
  }
  resp_time = utils::head(extremes_time,-1);#extremes_time[-1];

  # interpolate signal
  interpolate_singal = stats::spline(x=resp_time, y=resp,n = 10*length(resp_time))

  # create a plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = interpolate_singal$x, y = interpolate_singal$y)) +
    ggplot2::labs(title = "Respiration estimation", x = "Time[s]", y = "Value")

  # create components list
  signal_object <- list(signal = interpolate_singal$y, time = interpolate_singal$x,
                        signal_type="Respiration estimation", graph = p);
  # create an object of class Signal
  class(signal_object) <- "Signal"

  return(signal_object)
}
