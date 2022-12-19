
#' Birthday Card GIF
#'
#' @description Create a birthday card in GIF format
#' @param greeting the greeting words to show on the card, default is "Happy Birthday".
#' @param color the color of the birthday card, could be choose from "cream",
#' "chocolate" and "strawberry",default is "cream"
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


#' Birthday Video
#'
#' @param birthdaycard the GIF birthday card
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
#' @importFrom dplyr tibble mutate %>%
#' @examples
#' birthdaycard<-bday_card()
#' bday_video(birthdaycard)
bday_video<-function(birthdaycard, audio="bday music.mp3", duration = 13,
                         end_pause = 2){
  notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
  pitch <- "D D E D G F# D D E D A G D D D5 B G F# E C5 C5 B G A G"
  length1 <- c(rep(c(0.75, 0.25, 1, 1, 1, 2), 2),
               0.75, 0.25, 1, 1, 1, 1, 1, 0.75, 0.25, 1, 1, 1, 2)
  bday <- tibble(pitch = strsplit(pitch, " ")[[1]],
                 length1 = length1)
  bday <-bday %>% mutate(octave = substring(pitch, nchar(pitch)) %>%
                           {suppressWarnings(as.numeric(.))} %>%
                           ifelse(is.na(.), 4, .),
                         note = notes[substr(pitch, 1, 1)],
                         note = note + grepl("#", pitch) -
                           grepl("b", pitch) + octave * 12 +
                           12 * (note < 3),
                         freq = 2 ^ ((note - 60) / 12) * 440)

  tempo <- 120
  sample_rate <- 44100

  make_sine <- function(freq, length1) {
    wave <- sin(seq(0, length1 / tempo * 60, 1 / sample_rate) *
                  freq * 2 * pi)
    fade <- seq(0, 1, 50 / sample_rate)
    wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
  }

  bday_wave <-
    mapply(make_sine, bday$freq, bday$length1) %>%
    do.call("c", .)

  save.wave(bday_wave,"bday music.mp3")
  animate(birthdaycard,renderer = av_renderer('birthday_card_video.mp4',
          audio=audio),duration = duration, end_pause = end_pause)}

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
