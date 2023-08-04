library(openair)
library(tidyverse)
library(lubridate)


traj <- importTraj(site = "london", year = 2010)

traj <- readRDS("Datafile/traj/TrajData_ub_2020.rds")

head(traj)

selectByDate(traj,
             start = "15/12/2020",
             end = "17/12/2020"
) %>%
  trajPlot(
    map.cols = openColours("hue", 10),
    col = "grey30"
  )

traj$day <- as.Date(traj$date)

## plot it choosing a specific layout
selectByDate(traj,
             start = "15/12/2020",
             end = "21/12/2020"
) %>%
  trajPlot(
    type = "day",
    layout = c(7, 1)
  )


selectByDate(traj,
             start = "15/12/2020",
             end = "21/12/2020"
) %>%
  trajPlot(
    group = "day", col = "turbo",
    lwd = 2, key.pos = "top",
    key.col = 4,
    ylim = c(20, 55)
  )

kc1 <- importAURN("kc1", year = 2010)
env=fread("Datafile/FRIEND_1st_envi_re.csv")
env$day=as.Date(env$date)

env_ul=subset(env,env$group=="Ulaanbaatar")

# now merge with trajectory data by 'date'

traj2 <- inner_join(traj, env_ul[,-c(1:5,27)], by = "day")

dim(traj)
dim(traj2)

## look at first few lines
head(traj)
head(traj2)

selectByDate(traj2,
             start = "16/12/2020",
             end = "21/12/2020"
) %>%
  trajPlot(
    pollutant = "pm2.5",
    col = "turbo", lwd = 2
  )

scatterPlot(traj2, x = "oc", 
            y = "pm2.5", 
            avg = "day", 
            linear = TRUE)


filter(traj2, lat > 20, lat < 55, lon > 100, lon < 140) %>%
  trajLevel(
    method = "hexbin", col = "turbo",
    xbin = 40
  )


trajLevel(traj2, pollutant = "pm2.5", 
          statistic = "difference",
          col = c("skyblue", "white", "tomato"), 
          min.bin = 50, 
          border = NA, 
          xlim = c(100, 140), 
          ylim = c(20, 55))

trajLevel(traj2, 
          pollutant = "pm2.5", 
          statistic = "frequency", 
          col = "heat",
          type = "season",
          xlim = c(100, 140), 
          ylim = c(20, 55))



alloc <- traj2

id <- which(alloc$hour.inc == 0) 
y0 <- alloc$lat[id[1]]
x0 <- alloc$lon[id[1]]


alloc <- mutate(
  alloc, 
  angle = atan2(lon - x0, lat - y0) * 360 / 2 / pi,
  angle = ifelse(angle < 0, angle + 360 , angle),
  sector = cut(angle, 
               breaks = seq(22.5, 382.5, 45),
               labels = c("NE", "E", "SE", 
                          "S", "SW", "W",
                          "NW", "N")),
  sector = as.character(sector),
  sector = ifelse(is.na(sector), "N", sector)
) 


alloc <- group_by(alloc, date, sector) %>% 
  mutate(n = n()) %>% 
  group_by(date) %>% 
  arrange(date, n) %>% 
  slice_tail(n = 1) %>% 
  mutate(sector = ifelse(n > 50, sector, "unallocated")) %>% 
  dplyr::select(date, sector, n)

# combine with trajectories
traj3 <- left_join(traj2, alloc, by = "date")

head(traj3)

group_by(traj3, sector) %>% 
  summarise(PM2.5 = mean(pm2.5, na.rm = TRUE))

group_by(traj3, sector) %>% 
  summarise(n = n()) %>% 
  mutate(percent = 100 * n / nrow(traj3))

sp

filter(traj3, lon > 100, lon < 140, lat > 20, lat < 60) %>%
  trajLevel(
    pollutant = "pm2.5", statistic = "pscf",
    col = "increment",
    border = NA
  )

tt=filter(traj3, lat > 20 & lat < 55 & lon > 100 & lon < 140)

tt

filter(traj3,  lat > 20 & lat < 55 & lon > 100 & lon < 140) %>%
  trajLevel(
    pollutant = "pm2.5",
    statistic = "cwt",
    smooth = TRUE,
    col = "increment"
  )

?trajLevel



filter(traj3, lat > 20 & lat < 55 & lon > 100 & lon < 140) %>%
  trajLevel(
    pollutant = "pm2.5",
    statistic = "cwt",
    col = "increment",
    border = "white",
    lon.inc = 1,
    lat.inc = 1,
    orientation = c(0, 0, 0)
  )

trajLevel(traj3,
          pollutant = "pm2.5",
          statistic =  "sqtba",
          map.fill = FALSE,
          cols = "default",
          lat.inc = 0.5,
          lon.inc = 0.5
)

install.packages("openairmaps")
library(openairmaps)

traj_data

trajMap(traj_data, colour = "pm2.5")
clustdata <- trajCluster(traj_data)



trajMap(traj3, colour = "pm2.5")
clustdata <- trajCluster(traj2)

trajMap(
  data = clustdata$data$traj,
  colour = "cluster",
  control = "cluster",
  provider = "CartoDB.Positron"
)


networkMap(
  source = "aurn",
  control = NULL,
  date = Sys.Date(),
  cluster = TRUE,
  provider = c("OpenStreetMap", "Esri.WorldImagery"),
  collapse.control = FALSE
)

