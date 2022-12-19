## code to prepare `DATASET` dataset goes here

usethis::use_data(birthday_data, compress = "xz", overwrite = TRUE)
readr::write_csv(birthday_data, path = "inst/extdata/birthday_data.csv")

usethis::use_data(christmas_data, compress = "xz")
readr::write_csv(christmas_data, path = "inst/extdata/christmas_data.csv")


bday_music<-readMP3('bday_music.mp3')
usethis::use_data(bday_music_mp3, overwrite = TRUE)

xmas_music<-readMP3("xmas_music.mp3")
usethis::use_data(xmas_music_mp3, overwrite = TRUE)

library(readr)
library(tuneR)
library(av)
play(bday_music)


bday_music<-system.file('bday_music.mp3', package="gm")

av_media_info(bday_music)
system.file
list.files()


bday_music<-av_audio_convert('bday_music.mp3', 'bday_music.wav', channels = 1, total_time = 12)
bday_music_mp3<-av_audio_convert('bday_music.wav', 'bday_music.mp3', channels = 1, total_time = 12)

xmas_music<-av_audio_convert('xmas_music.mp3', 'xmas_music.wav', channels = 1, total_time = 12)
xmas_music_mp3<-av_audio_convert('xmas_music.wav', 'xmas_music.mp3', channels = 1, total_time = 12)
