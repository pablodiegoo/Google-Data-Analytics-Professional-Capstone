# Load libraries
library(tidyverse)
library(DBI)
library(RMySQL)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggmap)
library(sf)
library(geosphere)

# Push all CSV into mySQL database

# Connect to mySQL
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      dbname = sub(",$", "", Sys.getenv("DB_NAME")),
                      host = sub(",$", "", Sys.getenv("DB_HOST")),
                      username = sub(",$", "", Sys.getenv("DB_USER")),
                      password = sub(",$", "", Sys.getenv("DB_PSWD")))

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load csv  from stations
dbWriteTable(con, "stations", read.csv("Divvy_Bicycle_Stations_20250317.csv"), overwrite = TRUE)
dbExecute(con, "
    ALTER TABLE stations
    CHANGE `Station.Name` s_name VARCHAR(64),
    CHANGE `Short.Name` s_id VARCHAR(64),
    CHANGE `Latitude` lat FLOAT,
    CHANGE `Longitude` lng FLOAT,
    DROP COLUMN ID,
    DROP COLUMN `Total.Docks`,
    DROP COLUMN `Docks.in.Service,
    DROP COLUMN `Status`;")

# Load all CSV files
files <- list.files(pattern = "202[4-5]{1}[0-1]{1}[0-9]{1}-divvy-tripdata.csv")

# Extract all csv
for (file in files) {
  table_name <- paste0("t_", gsub("-divvy-tripdata.csv", "", file))
  dbWriteTable(con, table_name, read.csv(file), overwrite = TRUE)
}

# Process - Data Wrangling
dbExecute(con, "
    -- Create table t_base by merging all monthly tables
    CREATE TABLE t_base AS
    SELECT * FROM t_202401
    UNION ALL SELECT * FROM t_202402
    UNION ALL SELECT * FROM t_202403
    UNION ALL SELECT * FROM t_202404
    UNION ALL SELECT * FROM t_202405
    UNION ALL SELECT * FROM t_202406
    UNION ALL SELECT * FROM t_202407
    UNION ALL SELECT * FROM t_202408
    UNION ALL SELECT * FROM t_202409
    UNION ALL SELECT * FROM t_202410
    UNION ALL SELECT * FROM t_202411
    UNION ALL SELECT * FROM t_202412
    UNION ALL SELECT * FROM t_202501;");

dbExecute(con, "
    -- Fixing column date types
    ALTER TABLE t_base
    MODIFY started_at DATETIME,
    MODIFY ended_at DATETIME;"); # +5999257 lines

dbExecute(con, "
    -- Removing duplicate records
    DELETE t_base FROM t_base
    INNER JOIN (
        SELECT ride_id FROM t_base GROUP BY ride_id HAVING COUNT(*) > 1
    ) AS temp_duplicados
    ON t_base.ride_id = temp_duplicados.ride_id;"); # -422 Lines

dbExecute(con, "
    -- Removing records with missing values
    DELETE FROM t_base WHERE 
        ride_id IS NULL OR
        rideable_type IS NULL OR
        started_at IS NULL OR 
        ended_at IS NULL OR 
        member_casual IS NULL OR
        start_lat IS NULL OR
        start_lng IS NULL OR
        end_lat IS NULL OR
        end_lng IS NULL;"); # -7213 lines

dbExecute(con, "
    -- Removing records with missing values
    DELETE FROM t_base WHERE 
        start_station_name = '' AND
        end_station_name = '';"); # 535795 lines

dbExecute(con, "
    -- Removing negative duration
    DELETE FROM t_base WHERE started_at > ended_at;") # - 132 lines

dbExecute(con, "
    -- Fixing station name
    UPDATE t_base tb
    INNER JOIN stations s ON tb.start_station_name = s.s_name
    SET tb.start_station_id = s.s_id,
        tb.start_lat = s.lat,
        tb.start_lng = s.lng;"); # 0 lines

# query how many nulls on each column
null_counts <- dbGetQuery(con, "
  SELECT 
    SUM(CASE WHEN member_casual IS NULL THEN 1 ELSE 0 END) AS member_casual_nulls,
    SUM(CASE WHEN rideable_type IS NULL THEN 1 ELSE 0 END) AS rideable_type_nulls,
    SUM(CASE WHEN duration IS NULL THEN 1 ELSE 0 END) AS duration_nulls,
    SUM(CASE WHEN start_station_name IS NULL THEN 1 ELSE 0 END) AS start_station_name_nulls,
    SUM(CASE WHEN start_station_id IS NULL THEN 1 ELSE 0 END) AS start_station_id_nulls,
    SUM(CASE WHEN start_lat IS NULL THEN 1 ELSE 0 END) AS start_lat_nulls,
    SUM(CASE WHEN start_lng IS NULL THEN 1 ELSE 0 END) AS start_lng_nulls,
    SUM(CASE WHEN end_station_name IS NULL THEN 1 ELSE 0 END) AS end_station_name_nulls,
    SUM(CASE WHEN end_station_id IS NULL THEN 1 ELSE 0 END) AS end_station_id_nulls,
    SUM(CASE WHEN end_lat IS NULL THEN 1 ELSE 0 END) AS end_lat_nulls,
    SUM(CASE WHEN end_lng IS NULL THEN 1 ELSE 0 END) AS end_lng_nulls
  FROM t_base;
")
print(null_counts)

dbExecute(con, "
    -- Creating new transformed table
    CREATE TABLE t_data AS
    SELECT 
        CASE 
            WHEN member_casual = 'member' THEN 1
            ELSE 0
        END AS member_casual,
        CASE 
            WHEN rideable_type = 'electric_bike' THEN 1
            WHEN rideable_type = 'classic_bike' THEN 2
            ELSE 3
        END AS rideable_type,
        TIMESTAMPDIFF(SECOND, started_at, ended_at) AS duration,
        ROUND(ST_Distance_Sphere(
          POINT(start_lng, start_lat),
          POINT(end_lng, end_lat)
        )) AS distance,
        HOUR(started_at) AS hour,
        DAYOFWEEK(started_at) AS weekday,
        MONTH(started_at) AS month,
        YEAR(started_at) AS year,
        start_station_name,
        start_station_id,
        end_station_name,
        end_station_id,
        start_lat,
        start_lng,
        end_lat,
        end_lng
    FROM t_base;"); # total size 818mb


dbExecute(con, "
    -- Fixing column types
    ALTER TABLE t_data
    MODIFY member_casual TINYINT,
    MODIFY rideable_type TINYINT,
    MODIFY duration INT,
    MODIFY distance INT,
    MODIFY hour TINYINT,
    MODIFY weekday TINYINT,
    MODIFY month TINYINT,
    MODIFY year SMALLINT,
    MODIFY start_station_name VARCHAR(64),
    MODIFY end_station_name VARCHAR(64),
    MODIFY start_station_id VARCHAR(64),
    MODIFY end_station_id VARCHAR(64),
    MODIFY start_lat FLOAT,
    MODIFY start_lng FLOAT,
    MODIFY end_lat FLOAT,
    MODIFY end_lng FLOAT;"); # total size 671mb

dbExecute(con, "
    -- Removing negative duration
    DELETE FROM t_data WHERE duration <= 60;") # - 68716 lines

dbExecute(con, "
-- Removing Z score > 3 and < -3
DELETE FROM t_data
WHERE duration IN (
    SELECT duration FROM (
        SELECT 
            td.duration,
            (td.duration - avg_td.mean) / avg_td.stddev AS z_score
        FROM 
            t_data td,
            (SELECT 
                AVG(duration) AS mean, 
                STDDEV(duration) AS stddev 
             FROM t_data) AS avg_td
        HAVING ABS(z_score) > 3
    ) AS outliers
);") # - 35660 lines


# Stations
dbWriteTable(con, "stations", read.csv("Divvy_Bicycle_Stations_20250317.csv"), overwrite = TRUE)
dbExecute(con, "
    ALTER TABLE stations
    CHANGE `Station.Name` s_name VARCHAR(64),
    CHANGE `Short.Name` s_id VARCHAR(64),
    CHANGE `Latitude` lat FLOAT,
    CHANGE `Longitude` lng FLOAT,
    DROP COLUMN ID,
    DROP COLUMN `Total.Docks`,
    DROP COLUMN `Docks.in.Service,
    DROP COLUMN `Status`;")

# Create new tables
dbExecute(con, "
  CREATE TABLE data_time AS
  SELECT 
      CASE 
          WHEN member_casual = 1 AND rideable_type = 1 THEN 'M-Electric Bike'
          WHEN member_casual = 1 AND rideable_type = 2 THEN 'M-Classic Bike'
          WHEN member_casual = 1 AND rideable_type = 3 THEN 'M-Electric Scooter'
          WHEN member_casual = 0 AND rideable_type = 1 THEN 'C-Electric Bike'
          WHEN member_casual = 0 AND rideable_type = 2 THEN 'C-Classic Bike'
          ELSE 'C-Electric Scooter'
      END AS type,
      year,
      month,
      weekday,
      hour,
      member_casual,
      rideable_type,
      COUNT(*) AS n_trips,
      SUM(duration) AS duration,
      SUM(duration) / COUNT(*) AS avg_duration
  FROM t_data
  GROUP BY year, month, weekday, hour, type, member_casual, rideable_type
  ORDER BY year, month, weekday, hour, type;") # 9009 lines

dbExecute(con, "
  CREATE TABLE trip AS
  SELECT 
      member_casual,
      start_station_name,
      end_station_name,
      COUNT(*) AS n_trips,
      AVG(duration) AS duration,
      AVG(start_lat) AS s_lat,
      AVG(start_lng) AS s_lng,
      AVG(end_lat) AS e_lat,
      AVG(end_lng) AS e_lng
  FROM t_data
  GROUP BY member_casual, start_station_name, end_station_name;") #  273595 lines

# Data manipulation for ABS(Z-SCORE) >= 3 and remove duration < 60 seconds
data <- t_data %>%
  select(rideable_type, member_casual, duration) %>%
  mutate(trip_duration = round(as.numeric(duration)/60, 0))

dbExecute(con,"
  CREATE TABLE data AS
  SELECT 
      rideable_type,
      member_casual,
      ROUND(duration/60) AS duration,
      distance
  FROM t_data;")

# Load data
t_data <- DBI::dbGetQuery(con, "SELECT * FROM t_data")
trip <- DBI::dbGetQuery(con, "SELECT * FROM trip")
data_time <- DBI::dbGetQuery(con, "SELECT * FROM data_time")
data <- DBI::dbGetQuery(con, "SELECT * FROM data")
stations <- dbExecute(con, "SELECT * FROM stations") # Downloaded at https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations/bbyy-e7gq/about_data



# Histogram: Number of trips by trip duration
data %>%
  ggplot(aes(x = trip_duration, color = rideable_type, fill = rideable_type)) +
  geom_histogram(alpha = 0.6, binwidth = 2) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +  
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
    labs(title = "Histogram of Trips by Member Type",
       subtitle = "January 2024 to January 2025",
       x = "Trip Duration (minutes)",
       y = "Total of Trips") +
  facet_wrap(vars(member_casual), nrow = 1)
ggsave("dataviz/histogram.jpeg", width = 10, height = 6, units = "in")

# Histogram of number of trips by trip duration (Facets)
data %>%
  ggplot(aes(x = trip_duration, color = rideable_type, fill = rideable_type)) +
  geom_histogram(alpha = 0.6, binwidth = 2) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +  
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
    labs(title = "Histogram of Trips by Member Type",
       subtitle = "January 2024 to January 2025",
       x = "Trip Duration (minutes)",
       y = "Total of Trips") +
  facet_wrap(vars(rideable_type,member_casual), nrow = 3)
ggsave("dataviz/histogramfacet.jpeg", width = 10, height = 6, units = "in")

# Frequecy of trips by rideable type
data %>% 
  count(member_casual, rideable_type, name="n_trips") %>% 
  ggplot(aes(x = member_casual, y = n_trips, fill = rideable_type)) +
  geom_col(alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Total of Trips by Member Type",
       subtitle = "January 2024 to January 2025",
       x = "Type of user",
       y = "Total of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) ) +
  scale_y_continuous(labels = scales::comma)
ggsave("dataviz/frequency_bar.jpeg", width = 10, height = 6, units = "in")

# Box plot for trip duration
data %>%
  ggplot(aes(x = member_casual, y = trip_duration, fill = rideable_type)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Type of User") +
  ylab("Trip Duration") +
  facet_wrap(vars(rideable_type), nrow = 3)
ggsave("dataviz/boxplot.jpeg", width = 10, height = 6, units = "in")

# Percentile of trips by rideable type
data %>% 
  count(member_casual, rideable_type, name="n_trips") %>%
  mutate(type = case_when(member_casual == "member" & rideable_type == "electric_bike" ~ "M-Eletric Bike", member_casual == "member" & rideable_type == "classic_bike" ~ "M-Classic Bike", member_casual == "member" & rideable_type == "electric_scooter" ~ "M-Eletric Scooter", member_casual == "casual" & rideable_type == "electric_bike" ~ "C-Eletric Bike", member_casual == "casual" & rideable_type == "classic_bike" ~ "C-Classic Bike", member_casual == "casual" & rideable_type == "electric_scooter" ~ "C-Eletric Scooter")) %>%
  arrange(desc(type)) %>%
  mutate(percent = n_trips / sum(n_trips) * 100, pct_label = round(as.numeric(percent), 2),ymax = cumsum(percent), ymin = c(0, head(ymax, n=-1)),labpos = (ymax+ymin)/2) %>%
  ggplot(aes(x = 2, y = percent, fill = type)) +
  geom_bar(width = 1, stat = "identity", alpha = 0.6) +
  geom_text(aes(y=labpos, label=paste0(type,"\n",pct_label ," %")), size=3,alpha = 0.6) +
  coord_polar("y", start = 0) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Percentile of Trips by Rideable Type",
       subtitle = "January 2024 to January 2025",
       x = "",
       y = "",
       fill = "Rideable Type") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right") +
  xlim(0.5, 2.5)
ggsave("dataviz/piechart.jpeg", width = 10, height = 6, units = "in")

# Use analysis percentage
data_time  %>%
  mutate(date = paste(month,year, sep=".")) %>%
    mutate(date = factor(date, levels = c("Jan.2024", "Feb.2024", "Mar.2024", "Apr.2024", "May.2024","Jun.2024", "Jul.2024", "Aug.2024", "Sep.2024", "Oct.2024","Nov.2024", "Dec.2024", "Jan.2025"))) %>%
  group_by(date,member_casual,rideable_type) %>%
  summarise(n = sum(n_trips)) %>%
  mutate(percentage = round(as.numeric(n/sum(n)*100),2)) %>%
  ggplot(aes(x=date, y=percentage, fill=rideable_type)) +  
    geom_bar(stat="identity", position="stack",alpha=0.6 , size=.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal() +
  theme(
    legend.position = "top",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+   
  labs(x="Month", y="Percentage",
       title="Usage by user type",
       subtitle="Number of Travels percentage",
       caption="Brought to you by the letter 'g'") +
  facet_wrap(vars(member_casual), nrow = 2)
ggsave("dataviz/stackedBar.jpeg", width = 10, height = 6, units = "in")

# Use analysis Month
data_time  %>%
  mutate(date = paste(month,year, sep=".")) %>%
  mutate(date = factor(date, levels = c("Jan.2024", "Feb.2024", "Mar.2024", "Apr.2024", "May.2024","Jun.2024", "Jul.2024", "Aug.2024", "Sep.2024", "Oct.2024","Nov.2024", "Dec.2024", "Jan.2025"))) %>%
  group_by(date,member_casual,rideable_type) %>%
  summarise(n = sum(n_trips)) %>%
  ggplot(aes(x=date, y=n, fill=rideable_type)) +  
    geom_bar(stat="identity", position="stack",alpha=0.6 , size=.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal() +
  theme(
    legend.position = "top",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+   
  labs(x="Month", y="Total of Trips",
       title="Usage by user type",
       subtitle="Number of Trips",
       caption="Brought to you by the letter 'g'") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(vars(member_casual), nrow = 1)
ggsave("dataviz/stackedBarMonth.jpeg", width = 10, height = 6, units = "in")

# Use analysis Weekday
data_time  %>%
  group_by(day_of_week,member_casual,rideable_type) %>%
  summarise(n = sum(n_trips)) %>%
  ggplot(aes(x=day_of_week, y=n, fill=rideable_type)) +  
    geom_bar(stat="identity", position="stack",alpha=0.6 , size=.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal() +
  theme(
    legend.position = "top",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+   
  labs(x="Week Day", y="Total of Trips",
       title="Usage by user type",
       subtitle="Number of Trips",
       caption="Brought to you by the letter 'g'") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(vars(member_casual), nrow = 1)
ggsave("dataviz/stackedBarWeekday.jpeg", width = 10, height = 6, units = "in")

# Use analysis Hour
data_time  %>%
  group_by(hour_of_day,member_casual,rideable_type) %>%
  summarise(n = sum(n_trips)) %>%
  ggplot(aes(x=hour_of_day, y=n, fill=rideable_type)) +  
    geom_bar(stat="identity", position="stack",alpha=0.6 , size=.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal() +
  theme(
    legend.position = "top",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+   
  labs(x="Hour of Day", y="Total of Trips",
       title="Usage by user type",
       subtitle="Number of Trips",
       caption="Brought to you by the letter 'g'") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(vars(member_casual), nrow = 1)
ggsave("dataviz/stackedBarHour.jpeg", width = 10, height = 6, units = "in")

# Use heatmap (Weekday x Hour)
data_time  %>%
  mutate(date = paste(month,year, sep=".")) %>%
  mutate(date = factor(date, levels = c("Jan.2024", "Feb.2024", "Mar.2024", "Apr.2024", "May.2024","Jun.2024", "Jul.2024", "Aug.2024", "Sep.2024", "Oct.2024","Nov.2024", "Dec.2024", "Jan.2025"))) %>%
  group_by(date,day_of_week,hour_of_day,member_casual) %>%
  summarise(n = sum(n_trips)) %>%
  mutate(percentage = round(as.numeric(n/sum(n)*100),2)) %>%
  ggplot(aes(x = day_of_week, y = hour_of_day, fill = n)) +
    viridis::scale_fill_viridis(name="Divvy Rides",
                      option = 'C',
                      direction = 1,
                      na.value = "grey93") +
    geom_tile(color = 'white', size = 0.001) +
    facet_wrap(vars(date,member_casual), ncol=2) +
    theme_minimal() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+   
  labs(x="Week Day", y=element_blank(),
       title="Usage by user type",
       subtitle="Number of Travels by weekday and hour",
       caption=element_blank())+
  scale_y_reverse(breaks = seq(0, 23, by = 1), labels = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"))
ggsave("dataviz/heatmapWH.jpeg", width = 10, height = 40, units = "in")

# Map of stations and trip count
stations %>%
  group_by(start_station_name,member_casual) %>%
  filter(n_trips>0) %>%
  reframe(n_trips,lng,lat) %>%
  ggplot() +
  geom_sf(data = chicago_map, aes(), fill = NA) +
  geom_point(aes(x = lng, y = lat, size = n_trips/1000, color=n_trips/1000, alpha= n_trips/1000)) +
  labs(title = "Total Trips by Member Type",
       subtitle = "January 2024 to January 2025 over 100 trips per station",
       x = "Type of user",
       y = element_blank(),
       fill = element_blank()) +
  theme_minimal() +
  facet_wrap(vars(member_casual)) +
  scale_size_continuous(
    name = "Number of Trips (Thousands)", trans = "log",
    range = c(0.1, 4), breaks = c(0.1, 1, 5, 35)
  ) +
  scale_alpha_continuous(
    name = "Number of Trips (Thousands)", trans = "log",
    range = c(0.1, 1), breaks = c(0.1, 1, 5, 35)
  ) +
  scale_color_viridis_c(
    trans = "log",
    breaks = c(0.1, 1, 5, 35), name = "Number of Trips (Thousands)"
  ) +
  guides(colour = guide_legend()) +
ggsave("dataviz/geomap.jpeg", width = 10, height = 6, units = "in")


# Get chicago map & cords
chicago_map <- st_read(
  "https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson"
)

# Perform a spatial join to count trips per community area
stations_sf <- st_as_sf(stations, coords = c("lng", "lat"), crs = 4326)
stations_joined <- st_join(stations_sf, chicago_map, join = st_within)

# Summarize the number of trips per community area
trips_by_community <- stations_joined %>%
  group_by(community) %>%
  summarise(n_trips = sum(n_trips, na.rm = TRUE))

# Merge the trip data with the Chicago map
chicago_map <- merge(
  as.data.frame(chicago_map), trips_by_community, by = "community", all.x = TRUE
)

# Fill NA values in n_trips with 0
chicago_map$n_trips[is.na(chicago_map$n_trips)] <- 0

# Plot the choropleth map
ggplot(chicago_map) +
  geom_sf(aes(fill = n_trips), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  labs(
    title = "Number of Trips by Chicago Community Area",
    fill = "Number of Trips"
  ) +
  theme_minimal()
ggsave("dataviz/choropleth_map.jpeg", width = 10, height = 6, units = "in")

# Average minutes spent in each type of rideable separated by member type using a line chart
data_time %>%
  group_by(type, day_of_week) %>%
  summarise(avg_duration = trip_duration/n_trips) %>%
  ggplot(aes(x = avg_duration, y = day_of_week, color = type)) +
  geom_line() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Average Trip Duration by Member Type",
       subtitle = "January 2024 to January 2025",
       x = "Week Day",
       y = "Average Trip Duration (minutes)",
       fill = "Rideable Type") +
  theme_minimal()
ggsave("dataviz/avg_duration.jpeg", width = 10, height = 6, units = "in")