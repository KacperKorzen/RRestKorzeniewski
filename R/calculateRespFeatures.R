#' Calculate RRV parameters of the Respiration Signal
#' Function calculate the time domain parameters of RRV respiratory signal from a
#' respiration curve.
#'
#' @param resp Class Signal object with respiratory signal (output of 'ppgToResp' function)
#' @param mRR_bool Specifies if the mRR parameter to be calculated.
#' @param stdRR_bool Specifies if the stdRR parameter to be calculated.
#' @param RMSSD_bool  Specifies if the RMSD parameter to be calculated.
#
#' @return A S3 class "Signal" object which containing information about a signal
#'         from param 'resp', with RRV parameters mRR, stdRR and RMSSD
#'         The "mRR" attribute holds the average length of breathing, while "stdRR"
#'         standard deviation of the differences between the durations of adjacent breaths
#'         and RMSSD atribute which contains information about root mean square of
#'         the differences between the durations of adjacent breaths.
#'
#' @examples
#'           calculateRespFeatures(resp = respObject);
#'           calculateRespFeatures(resp = respObject, mRR_bool = TRUE, stdRR_bool = FALSE, RMSSD_bool = TRUE);
#'           calculateRespFeatures(resp = respObject, mRR_bool = FALSE, stdRR_bool = TRUE, RMSSD_bool = TRUE);
#'           calculateRespFeatures(resp = respObject, mRR_bool = FALSE, stdRR_bool = FALSE, RMSSD_bool = TRUE);
#'           calculateRespFeatures(resp = respObject, mRR_bool = TRUE, stdRR_bool = FALSE, RMSSD_bool = FALSE);
#'
#' @export calculateRespFeatures

calculateRespFeatures <- function(resp, mRR_bool = TRUE, stdRR_bool = TRUE, RMSSD_bool = TRUE){
  # input checking
  stopifnot(class(resp) == "Signal");
  stopifnot(is.numeric(resp$signal));
  stopifnot(is.numeric(resp$time));
  stopifnot(is.logical(mRR_bool));
  stopifnot(is.logical(stdRR_bool));
  stopifnot(is.logical(RMSSD_bool));
  if (mRR_bool == FALSE & stdRR_bool == FALSE & RMSSD_bool == FALSE){
    stop('Warning: No Feature defined for calculation!')
  }

  # get extremes
  extremes <- gsignal::findpeaks(resp$signal, DoubleSided = TRUE);
  if(length(extremes$pks)<3){
    stop('Error: There was not enought extrem points!');
  }

  # checking if first extremes is min
  if(extremes$pks[1] > extremes$pks[2]){
    extremes$pks = utils::tail(extremes$pks, -1);
    extremes$loc = utils::tail(extremes$loc, -1);
    if(length(extremes$pks)<3){
      stop('Error: There was not enought extrem points!');
    }
  }

  # checking if vector of extremes begin from min and end at min
  if(length(extremes$pks)%%2 == 0){
    extremes$pks = utils::head(extremes$pks, -1);
    extremes$loc = utils::head(extremes$loc, -1);
  }

  # update plot
  resp$graph <- resp$graph +
    ggplot2::geom_point(ggplot2::aes(x = resp$time[extremes$loc], y = extremes$pks), color = "blue")
  # it always start from local minimum and end with local minimum
  extremes_time = resp$time[extremes$loc]

  #calculate mean length of respiration
  if(mRR_bool == TRUE){
    mRR = mean(diff(extremes_time[seq(1, length(extremes_time),by = 2)]));
    resp$mRR = mRR;
  }

  # calculate std of different of length of respiration
  if(stdRR_bool == TRUE){
    stdRR = pracma::std(diff(diff(extremes_time[seq(1, length(extremes_time),by = 2)])));
    resp$stdRR = stdRR;
  }

  # calculate RMSSD
  if(RMSSD_bool == TRUE){
    square_value = (diff(diff(extremes_time[seq(1, length(extremes_time),by = 2)])))^2;
    RMSSD = sqrt(sum(square_value)/length((square_value)));
    resp$RMSSD = RMSSD;
  }

  return(resp)
}
