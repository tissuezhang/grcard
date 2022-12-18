#' Birthday Card Video
#'
#' @param birthdaycard the GIF birthday card witch created by \code{birthdaycard}
#' @param audio the audio play with the video
#' @param duration the duration of the video
#' @param end_pause the pause time at the end
#'
#' @return A video format of birthday card
#' @export
#' @importFrom gganimate av_renderer
#' @importFrom gganimate animate
#' @examples
birthday_video<-function(birthdaycard, audio="b_day_music.mp3", duration = 12,
                        end_pause = 2){
  animate(bdaycardgif,renderer = av_renderer('birthday_video.mp4',
  audio=audio),duration = duration, end_pause = end_pause)}
