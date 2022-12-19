
#' Christmas Card GIF
#'
#' @description Create a Christmas card in GIF format
#' @param greeting the greeting words to show on the card, defult is "Merry Christmas"
#' @param color the color of the Christmas card, could be choose from "forest",
#' "candy" and "dream", or create your own color combination by using. Defult is "candy"
#' the \code{xmas_color}
#'
#' @return A GIF format Christmas card
#' @export
#' @importFrom readxl read_excel
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme
#' @importFrom gganimate transition_states
#' @importFrom gganimate view_zoom
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes element_blank element_rect
#' @importFrom data.table data.table
#' @examples xmas_card()
xmas_card<-function(greeting="Merry Christmas",
                        color="candy"){
  x <- y <- group<-size<-t<- NULL
  christmas_data <- data.table::data.table(christmas_data)
  christmas<-ggplot(christmas_data, aes(x, y))+
    geom_point(aes(color=group, size=size))+
    transition_states(t,transition_length = 2, state_length = 5)+
    view_zoom(pause_length = 5, step_length = 2, nsteps = 2, pan_zoom = -3)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = 'none',
          panel.grid = element_blank(),
          plot.background = element_rect(fill = '#D16103'))
  if (color=="forest"){
    color<-c("#C4961A", "#F4EDCA", "brown","#C3D7A4","#00AFBB",
             "#293352", "#FC4E07",  "gold", "darkgreen",  "forestgreen",
             "olivedrab3", "yellow3", "chocolate4")
  }
  else if (color=="candy"){
    color<-c("#C4961A", "#F4EDCA", "brown","#C3D7A4","#00AFBB",
             "#293352", "#FC4E07",  "gold", "#E7B800",  "#D16103",
             "#52854C", "dodgerblue3", "chocolate4")
  }
  else if (color=="dream"){
    color<-c("gold", "rosybrown1","red", "deepskyblue1", "black", "blue",
             "orange", "hotpink",  "pink4","pink3",
             "pink2", "pink1", "snow4")
  }
  christmas<-christmas+labs(x= greeting)+scale_color_manual(values = color)
  christmas
}


#' Christmas Card Video
#'
#' @param christmascard the GIF Christmas card witch created by \code{xmas_card}
#' @param audio the audio play with the video
#' @param duration the duration of the video
#' @param end_pause the pause time at the end
#'
#' @return A video format of Christmas card
#' @export
#' @importFrom gganimate av_renderer
#' @importFrom gganimate animate
#' @importFrom av av_audio_convert
#' @examples
#' christmascard<-xmas_card(color="candy")
#' xmas_video(christmascard)
xmas_video<-function(christmascard, audio=xmas_music_mp3,
                     duration = 12,end_pause = 2){
  animate(christmascard,renderer = av_renderer('christmas_video.mp4',
                       audio=audio),duration = duration, end_pause = end_pause)}

#' Christmas Card Colors
#'
#' @param ball1 ball color
#' @param ball2 ball color
#' @param ball3 ball color
#' @param ball4 ball color
#' @param ball5 ball color
#' @param ball6 ball color
#' @param ball7 ball color
#' @param star star color
#' @param treebottom treebottom color
#' @param treelower treelower color
#' @param treeupper treeupper color
#' @param treetop treetop color
#' @param trunk trunk color
#'
#' @return a list of colors for the Christmas card
#' @export
#'
#'
xmas_color<-function(ball1, ball2, ball3, ball4,ball5, ball6,
                ball7, star, treebottom, treelower, treeupper, treetop, trunk)
{
  c(ball1, ball2, ball3, ball4,ball5, ball6,
    ball7, star, treebottom, treelower, treeupper, treetop, trunk)
}


#' @title Christmas Card Data

#' @description The data for plotting the Christmas card
#' @format A table with 5 columns, which are:
#' \describe{
#' \item{x}{the position of the points on there x-axis}
#' \item{y}{the position of the points on there y-axis}
#' \item{group}{the group of the point}
#' \item{size}{the size of the point}
#' \item{t}{the thansition step of the point}
#' }
"christmas_data"
