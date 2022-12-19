
#' Birthday Card GIF
#'
#' @description Create a birthday card in GIF format
#' @param greeting the greeting words to show on the card
#' @param color the color of the birthday card, could be choose from "cream",
#' "chocolate" and "strawberry", or create your own color combination by using
#' the \code{bdaycolor}
#'
#' @return A GIF format birthday card
#' @export
#' @importFrom readxl read_excel
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme
#' @importFrom gganimate transition_states
#' @importFrom gganimate view_zoom
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes element_blank element_rect
#' @importFrom data.table data.table
#' @examples bday_card()
bday_card<-function(greeting="Happy Birthday",
                   color="cream"){
  x <- y <- group<-size<-t<-shape <- NULL
  birthday_data <- data.table::data.table(birthday_data)
  birthday<-ggplot(birthday_data, aes(x, y))+
    geom_point(aes(color=group, size=size, shape=shape))+
    transition_states(t,transition_length = 2, state_length = 5)+
    scale_shape_manual(values=c(16, 8))+
    view_zoom(pause_length = 5, step_length = 2, nsteps = 2, pan_zoom = -3)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = 'none',
          panel.grid = element_blank(),
          plot.background = element_rect(fill = 'thistle'))
  if (color=="cream"){
    color<-c("#C4961A", "#F4EDCA", "brown","#C3D7A4","#00AFBB",
              "#293352", "#FC4E07",  "#0066CC", "#E7B800",  "#D16103",
              "#52854C", "dodgerblue3", "darkorange2")
  }
  else if (color=="chocolate"){
    color<-c("burlywood", "chocolate4","red", "deepskyblue1", "tomato3", "blue",
             "green4", "hotpink3",  "olivedrab","dodgerblue3",
             "firebrick3", "orange", "firebrick1")
  }
  else if (color=="strawberry"){
    color<-c("salmon", "rosybrown1","red", "deepskyblue1", "black", "blue",
             "orange", "hotpink3",  "pink3","hotpink4",
             "indianred4", "firebrick1", "tomato3")
  }
  birthday<-birthday+labs(x= greeting)+scale_color_manual(values = color)
  birthday
}

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
#' @importFrom av av_audio_convert
#' @examples
#' birthdaycard<-bday_card()
#' bday_video(birthdaycard)
bday_video<-function(birthdaycard, audio = bday_music_mp3, duration = 12,
                         end_pause = 2){
  animate(birthdaycard,renderer = av_renderer('birthday_video.mp4',
             audio=audio),duration = duration, end_pause = end_pause)}

#' Birthday Card Colors
#'
#' @param cakebottom cake bottom color
#' @param caketop cake top color
#' @param candle1 candle1 color
#' @param candle2 candle2 color
#' @param candle3 candle3 color
#' @param candle4 candle4 color
#' @param candle5 candle5 color
#' @param candle6 candle6 color
#' @param line1 line1 color
#' @param line2 line2 color
#' @param line3 line3 color
#' @param star1 star1 color
#' @param star2 star2 color
#'
#' @return the colors for the birthday card
#' @export
#'
#'
bday_color<-function(cakebottom, caketop, candle1, candle2,candle3, candle4,
                    candle5, candle6, line1, line2, line3, star1, star2)
{
  c(candle1, candle2,candle3, candle4, candle5, candle6,
    caketop, cakebottom, line1, line2, line3, star1, star2)
}


#' @title Birthday_card Data

#' @description The data for plotting the card
#' @format A table with 6 columns, which are:
#' \describe{
#' \item{x}{the position of the points on there x-axis}
#' \item{y}{the position of the points on there y-axis}
#' \item{group}{the group of the point}
#' \item{size}{the size of the point}
#' \item{t}{the thansition step of the point}
#' \item{shape}{the shape of the point}
#' }
"birthday_data"
