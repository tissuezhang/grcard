library(ggplot2)
library(tidyverse)
library(gganimate)
library(audio)
library(readxl)
library(gm)
library(av)
usethis::use_package("ggplot2")
usethis::use_package("gganimate")
usethis::use_package("readxl")
#' Birthday Card GIF
#'
#' @description Create a birthday card in GIF format
#' @param greeting the greeting words to show on the card
#' @param color the color of the birthday card, could be choose from "cream",
#' "chocolate" and "strawberry", or create your own color combination by using
#' the "bdaycolor" function
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
#' @importFrom ggplot2 aes
#'
#' @examples birthday_card()
birthday_card<-function(greeting="Happy Birthday",
                   color="cream"){
  birthday<-ggplot(read_excel("birthday_data.xlsx"), aes(x, y))+
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


