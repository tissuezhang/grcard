
#' Christmas Card GIF
#'
#' @description Create a Christmas card in GIF format
#' @param greeting the greeting words to show on the card, default is "Merry Christmas"
#' @param color the color of the Christmas card, could be choose from "forest",
#' "candy" and "dream", default is "candy".
#'
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


#' Christmas Video
#'
#' @param christmascard the GIF Christmas card witch created by \code{xmas_card}
#' @param audio the audio play with the video
#' @param duration the duration of the video
#' @param end_pause the pause time at the end
#'
#' @return A video format of birthday card
#' @export
#' @importFrom gganimate av_renderer
#' @importFrom gganimate animate
#' @importFrom av av_audio_convert
#' @importFrom audio save.wave
#' @importFrom dplyr tibble mutate

xmas_video<-function(christmascard, audio="xmas music.mp3", duration = 13,
                         end_pause = 2){
  notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
  pitch2 <- list("D","G","G","A","G","F#","E","C","C","A","A","B","A",
                 "G","F#","D","D","B","B","C5","B","A","G","E","D","E",
                 "A", "F#", "G")
  length2 <- c(1, 1, 0.5,0.5, 0.5, 0.5,1, 1, 1, 1,0.5, 0.5, 0.5,0.5,1, 1, 1,
                 1, 0.5,0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 3)
  xmas <- tibble(pitch = pitch2,
                 length2= length2)

  xmas <-xmas %>%mutate(octave = substring(pitch, nchar(pitch)) %>%
             {suppressWarnings(as.numeric(.))} %>%
             ifelse(is.na(.), 4, .),
           note = notes[substr(pitch, 1, 1)],
           note = note + grepl("#", pitch) -
             grepl("b", pitch) + octave * 12 +
             12 * (note < 3),
           freq = 2 ^ ((note - 60) / 12) * 440)

  tempo <- 120
  sample_rate <- 44100

  make_sine <- function(freq, length2) {
    wave <- sin(seq(0, length2/ tempo * 60, 1 / sample_rate) *
                  freq * 2 * pi)
    fade <- seq(0, 1, 50 / sample_rate)
    wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
  }

  xmas_wave <-
    mapply(make_sine, xmas$freq, xmas$length2) %>%
    do.call("c", .)
  save.wave(xmas_wave,"xmas music.mp3")

  animate(christmascard,renderer = av_renderer('card_video.mp4',
       audio=audio),duration = duration, end_pause = end_pause)}



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
