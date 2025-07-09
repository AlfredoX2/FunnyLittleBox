library(quicR)
library(dplyr)
library(plot3D)
library(plot3Drgl)
library(magick)


files <- list.files("raw", full.names = TRUE)
# file <- files[1]

curate <- function(file) {
  print(file)
  data <- get_real(file)[[1]] %>%
    normalize_RFU()
  
  meta <- organize_tables(file, plate = 384) %>%
    convert_tables()
  
  calcs <- data.frame(
    `Sample IDs` = meta$`Sample IDs`,
    check.names = FALSE
  ) %>%
    mutate(
      MPR = calculate_MPR(data),
      MS  = calculate_MS(data),
      TtT = calculate_TtT(data, 2),
      RAF = 1/TtT
    )
}

if (!file.exists("data.csv")) {
  df_ <- lapply(files, curate) %>%
    bind_rows()
  write.csv(df_, "data.csv", row.names=FALSE)
}else {df_=read.csv("data.csv")}



df_filt <- filter(df_, MPR > 2, RAF < 0.14)
scatter3D(
  x = df_filt$MS,
  y = df_filt$MPR,
  z = df_filt$RAF,
  scale = TRUE,
  xlab = "MS",
  ylab = "MPR", 
  zlab = "RAF",
  pch = 20,  theta = 10, phi = 10
)

plotrgl()
par3d(windowRect = c(20, 30, 800, 800))

movie3d(
  movie="3dAnimatedScatterplot", 
  spin3d( axis = c(0, 0, 1), rpm = 20,dev = cur3d()),
  startTime = 0,
  duration = 10, 
  dir = "images",
  type = "gif", 
  clean=FALSE,
  fps = 20,
)

list.files("images", ".png", full.names=TRUE) %>%
file.remove()
