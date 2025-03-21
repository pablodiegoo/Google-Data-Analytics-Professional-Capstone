#  Introduction

# This study is a part of Google Data Analytics Professional Certificate.
# The goal is to analyze the Divvy bike-share program following the six
# steps of the data analysis process: ask, prepare, process, analyze, share,
# and act.

# The study will take from january of 2024 to january of 2025 as the time
# frame to analyze the data. The data used in this study is from the Divvy
# bike-share program in Chicago. The data is available in CSV format and can
# be downloaded from the following links:
# [Datasets](https://divvy-tripdata.s3.amazonaws.com/index.html).

# The data is divided into one file for each month and includes information
# about the start and end time of the trip, the start and end stations, the
# duration of the trip, and the user type.

# I will use SQL to merge all data, so will be easier to analyze the data.
# All file will be downloaded and saved in the same folder as the script.

# - Import each into separate tables
# - Merge all the tables into one
# - Regularise data types
# - Inspect, Identify and Exclude  outliers and errors
# - Create queries for data visualisations

# Load libraries
library(tidyverse)
library(DBI)
library(RMySQL)

# Push all CSV into mySQL database

# Connect to mySQL
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      dbname = sub(",$", "", Sys.getenv("DB_NAME")),
                      host = sub(",$", "", Sys.getenv("DB_HOST")),
                      username = sub(",$", "", Sys.getenv("DB_USER")),
                      password = sub(",$", "", Sys.getenv("DB_PSWD")))

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load all CSV files into mySQL that are in the same folder as the script
# and with the format 20*-divvy-tripdata

files <- list.files(pattern = "202[4-5]{1}[0-1]{1}[0-9]{1}-divvy-tripdata.csv")
# Extract all csv to a single SQL table
for (file in files) {
  # Extract table name from file name
  table_name <- paste0("t_", gsub("-divvy-tripdata.csv", "", file))
  # Load CSV into mySQL
  dbWriteTable(con, table_name, read.csv(file), overwrite = TRUE)
}

# Process - Data Wrangling

# The data is now in a mySQL database. I will use SQL to wrangle the data,
# merging all tables into one and starting the data cleaning.

# Merge data
dbExecute(con, "
    CREATE TABLE t_data AS
    SELECT * FROM t_202401
    UNION ALL
    SELECT * FROM t_202402
    UNION ALL
    SELECT * FROM t_202403
    UNION ALL
    SELECT * FROM t_202404
    UNION ALL
    SELECT * FROM t_202405
    UNION ALL
    SELECT * FROM t_202406
    UNION ALL
    SELECT * FROM t_202407
    UNION ALL
    SELECT * FROM t_202408
    UNION ALL
    SELECT * FROM t_202409
    UNION ALL
    SELECT * FROM t_202410
    UNION ALL
    SELECT * FROM t_202411
    UNION ALL
    SELECT * FROM t_202412
    UNION ALL
    SELECT * FROM t_202501")

# Check if the Data type of columns are correct
print(dbGetQuery(con, "
    SELECT 
        COLUMN_NAME, 
        DATA_TYPE 
    FROM INFORMATION_SCHEMA.COLUMNS 
    WHERE TABLE NAME = 't_data'"))

# Fixing the columns type of started at and ended at to datetime
dbExecute(con, "
    ALTER TABLE t_data
    MODIFY row_names INT8,
    MODIFY ride_id VARCHAR(128),
    MODIFY rideable_type VARCHAR(128),
    MODIFY started_at DATETIME, 
    MODIFY ended_at DATETIME,
    MODIFY start_station_name VARCHAR(128),
    MODIFY start_station_id VARCHAR(128),
    MODIFY end_station_name VARCHAR(128),
    MODIFY end_station_id VARCHAR(128),
    MODIFY start_lat FLOAT,
    MODIFY start_lng FLOAT,
    MODIFY end_lat FLOAT,
    MODIFY end_lng FLOAT
    ")


# Cleaning Data - Delete duplicates
dbExecute(con, "
    CREATE TEMPORARY TABLE temp_duplicados AS
    SELECT ride_id
    FROM t_data
    GROUP BY ride_id
    HAVING COUNT(*) > 1;

    DELETE FROM t_data
    WHERE ride_id IN (SELECT ride_id FROM temp_duplicados);

    DROP TEMPORARY TABLE temp_duplicados;")

# Cleaning Data - Remove missing values
dbExecute(con, "
    DELETE FROM t_data 
    WHERE 
        ride_id IS NULL OR
        rideable_type IS NULL OR
        started_at IS NULL OR 
        ended_at IS NULL OR 
        member_casual IS NULL OR
        start_lat IS NULL OR
        end_lat IS NULL") # 7213 issues

# Add columns for trip duration, hour of the day, day of the week  and month
dbExecute(con, "
    ALTER TABLE t_data
    ADD COLUMN trip_duration INT,
    ADD COLUMN hour_of_day INT,
    ADD COLUMN day_of_week varchar(64),
    ADD COLUMN month varchar(64)")

# Copy progress to backup table
dbExecute(con, "
    CREATE TABLE t_backup AS SELECT * FROM t_data;")

dbExecute(con,"
    DROP TABLE t_data")

# create a new table, with columns member_casual, rideable_type, trip_duration (in seconds),
# hour_of_day, day_of_week (as Sun, Mon, Tue, Wed, Thu, Fri, Sat), and month (as Jan, Feb,
# Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec), year, start_station_name, end_station_name,
# start_point (as start_lat, start_lng), and end_point (as end_lat, end_lng).
dbExecute(con, "
    CREATE TABLE t_data AS
    SELECT 
        member_casual,
        rideable_type,
        TIMESTAMPDIFF(SECOND, started_at, ended_at) AS trip_duration,
        EXTRACT(HOUR FROM started_at) AS hour_of_day,
        CASE 
            WHEN DAYOFWEEK(started_at) = 1 THEN 'Sun'
            WHEN DAYOFWEEK(started_at) = 2 THEN 'Mon'
            WHEN DAYOFWEEK(started_at) = 3 THEN 'Tue'
            WHEN DAYOFWEEK(started_at) = 4 THEN 'Wed'
            WHEN DAYOFWEEK(started_at) = 5 THEN 'Thu'
            WHEN DAYOFWEEK(started_at) = 6 THEN 'Fri'
            WHEN DAYOFWEEK(started_at) = 7 THEN 'Sat'
        END AS day_of_week,
        CASE 
            WHEN MONTH(started_at) = 1 THEN 'Jan'
            WHEN MONTH(started_at) = 2 THEN 'Feb'
            WHEN MONTH(started_at) = 3 THEN 'Mar'
            WHEN MONTH(started_at) = 4 THEN 'Apr'
            WHEN MONTH(started_at) = 5 THEN 'May'
            WHEN MONTH(started_at) = 6 THEN 'Jun'
            WHEN MONTH(started_at) = 7 THEN 'Jul'
            WHEN MONTH(started_at) = 8 THEN 'Aug'
            WHEN MONTH(started_at) = 9 THEN 'Sep'
            WHEN MONTH(started_at) = 10 THEN 'Oct'
            WHEN MONTH(started_at) = 11 THEN 'Nov'
            WHEN MONTH(started_at) = 12 THEN 'Dec'
        END AS month,
        EXTRACT(YEAR FROM started_at) AS year,
        start_station_name,
        end_station_name,
        start_lat,
        start_lng,
        end_lat,
        end_lng
    FROM t_backup;")

# Removing Z score > 3 and < -3
dbExecute(con, "
DELETE FROM t_data
WHERE trip_duration IN (
    SELECT trip_duration FROM (
        SELECT 
            td.trip_duration,
            (td.trip_duration - avg_td.mean) / avg_td.stddev AS z_score
        FROM 
            t_data td,
            (SELECT 
                AVG(trip_duration) AS mean, 
                STDDEV(trip_duration) AS stddev 
             FROM t_data) AS avg_td
        HAVING ABS(z_score) > 3
    ) AS outliers
);")

# Cleaning Data - Removing Outliers

# Since our goal is answer "How do annual members and casual riders
# use Cyclistic bikes dierently?", we  will remove the row that
# started_at is greater than ended_at, the duration of the trip is
# greater than 24 hours and less than 1 minute.

####################################################################
# This section is desativated since i found many outliars and then I
# prefer to use z-score to remove now, but i will keep it here anyway
####################################################################
# # If the started at is greater than ended at
# dbExecute(con, "
#     DELETE FROM  t_data 
#     WHERE 
#         started_at >= ended_at") # 227 Issues
# 
# # If the duration of the trip is greater than 24 hours
# dbExecute(con, "
#     DELETE FROM  t_data 
#     WHERE 
#         TIMESTAMPDIFF(SECOND, started_at, ended_at) > 86400") # 380 issues
# 
# # If the duration of the trip is less than 1 minute
# dbExecute(con, "
#     DELETE FROM  t_data 
#     WHERE 
#         TIMESTAMPDIFF(SECOND, started_at, ended_at) < 60") # 133865 issues
####################################################################

# Make a dataframe that list the start stations id and start station names
# to check if there are stations with different IDs
id_start_fix <- dbGetQuery(con, "
    SELECT 
        start_station_name, 
        start_station_id,
        COUNT(*) AS n_trips 
    FROM t_data 
    GROUP BY start_station_name, start_station_id") %>% 
    group_by(start_station_name) %>% 
    summarise(
        n_distinct_ids = n_distinct(start_station_id),
        most_popular_id = start_station_id[which.max(n_trips)],
        less_popular_id = start_station_id[which.min(n_trips)]
    ) %>% 
    filter(n_distinct_ids > 1)

# Fix stations with different IDs by changing the ID to the most common one
# using a query
dbExecute(con, "
    UPDATE t_data
    SET start_station_id = (
        SELECT main_id
        FROM (
            SELECT
                start_station_name,
                CASE
                    WHEN COUNT(DISTINCT start_station_id) = 1 THEN MAX(start_station_id)
                    WHEN COUNT(DISTINCT start_station_id) > 1 THEN MAX(start_station_id)
                END AS main_id
            FROM t_data
            GROUP BY start_station_name
        ) AS MainID
        WHERE t_data.start_station_name = MainID.start_station_name
    )
    WHERE start_station_id != (
        SELECT main_id
        FROM (
            SELECT
                start_station_name,
                CASE
                    WHEN COUNT(DISTINCT start_station_id) = 1 THEN MAX(start_station_id)
                    WHEN COUNT(DISTINCT start_station_id) > 1 THEN MAX(start_station_id)
                END AS main_id
            FROM t_data
            GROUP BY start_station_name
        ) AS MainID
        WHERE t_data.start_station_name = MainID.start_station_name
    )") # 6949 issues

# Make a dataframe that list the end stations id and end station names
# to check if there are stations with different IDs
id_end_fix <- dbGetQuery(con, "
    SELECT 
        end_station_name, 
        end_station_id,
        COUNT(*) AS n_trips 
    FROM t_data 
    GROUP BY end_station_name, end_station_id") %>% 
    group_by(end_station_name) %>% 
    summarise(
        n_distinct_ids = n_distinct(end_station_id),
        most_popular_id = end_station_id[which.max(n_trips)],
        less_popular_id = end_station_id[which.min(n_trips)]
    ) %>% 
    filter(n_distinct_ids > 1)

# Fix stations with different IDs by changing the ID to the most common one
# using a query
dbExecute(con, "
    UPDATE t_data
    SET end_station_id = (
        SELECT main_id
        FROM (
            SELECT
                end_station_name,
                CASE
                    WHEN COUNT(DISTINCT end_station_id) = 1 THEN MAX(end_station_id)
                    WHEN COUNT(DISTINCT end_station_id) > 1 THEN MAX(end_station_id)
                END AS main_id
            FROM t_data
            GROUP BY end_station_name
        ) AS MainID
        WHERE t_data.end_station_name = MainID.end_station_name
    )
    WHERE end_station_id != (
        SELECT main_id
        FROM (
            SELECT
                end_station_name,
                CASE
                    WHEN COUNT(DISTINCT end_station_id) = 1 THEN MAX(end_station_id)
                    WHEN COUNT(DISTINCT end_station_id) > 1 THEN MAX(end_station_id)
                END AS main_id
            FROM t_data
            GROUP BY end_station_name
        ) AS MainID
        WHERE t_data.end_station_name = MainID.end_station_name
    )") # 6995 issues

# Make a dataframe that list the station stations names and start station ids
# to heck if there are stations with different names
name_start_fix <- dbGetQuery(con, "
    SELECT 
        start_station_name, 
        start_station_id,
        COUNT(*) AS n_trips 
    FROM t_data 
    WHERE 
        start_station_name NOT LIKE 'Public Rack - %'
    GROUP BY start_station_name, start_station_id") %>% # The filters where added after some checks
    group_by(start_station_id) %>%
    summarise(
        n_distinct_names = n_distinct(start_station_name),
        most_popular_name = start_station_name[which.max(n_trips)],
        less_popular_name = start_station_name[which.min(n_trips)]
    ) %>% 
    filter(n_distinct_names > 1)

# Fix stations with different names by changing the name to the most common one
# using a query
##################################################################
# Removed, need fixes.
##################################################################
# dbExecute(con, "
#     UPDATE t_data
#     SET start_station_name = (
#         SELECT main_name
#         FROM (
#             SELECT
#                 start_station_id,
#                 CASE
#                     WHEN COUNT(DISTINCT start_station_name) = 1 THEN MAX(start_station_name)
#                     WHEN COUNT(DISTINCT start_station_name) > 1 THEN MAX(start_station_name)
#                 END AS main_name
#             FROM t_data
#             WHERE 
#                 end_station_name NOT LIKE 'Public Rack%' OR
#                 end_station_name NOT LIKE '%TEMPORARY)
#             GROUP BY start_station_id
#         ) AS MainName
#         WHERE t_data.start_station_id = MainName.start_station_id
#     )
#     WHERE start_station_name != (
#         SELECT main_name
#         FROM (
#             SELECT
#                 start_station_id,
#                 CASE
#                     WHEN COUNT(DISTINCT start_station_name) = 1 THEN MAX(start_station_name)
#                     WHEN COUNT(DISTINCT start_station_name) > 1 THEN MAX(start_station_name)
#                 END AS main_name
#             FROM t_data
#             WHERE 
#                 end_station_name NOT LIKE 'Public Rack%' OR
#                 end_station_name NOT LIKE '%TEMPORARY)
#             GROUP BY start_station_id
#         ) AS MainName
#         WHERE t_data.start_station_id = MainName.start_station_id
#     )") # 134262 issues
##################################################################

# Make a dataframe that list the end stations names and end station ids
# to check if there are stations with different names
name_end_fix <- dbGetQuery(con, "
    SELECT 
        end_station_name, 
        end_station_id,
        COUNT(*) AS n_trips 
    FROM t_data 
    WHERE 
        end_station_name NOT LIKE 'Public Rack%' OR
        end_station_name NOT LIKE '%TEMPORARY)'
    GROUP BY end_station_name, end_station_id") %>% 
    group_by(end_station_id) %>% 
    summarise(
        n_distinct_names = n_distinct(end_station_name),
        count_trips = n_trips,
        most_popular_name = end_station_name[which.max(n_trips)],
        less_popular_name = end_station_name[which.min(n_trips)]
    ) %>% 
    filter(n_distinct_names > 1)

# Fix stations with different names by changing the name to the most common one
# using a query
##################################################################
# Removed, need fixes.
##################################################################
# dbExecute(con, "
#     UPDATE t_data
#     SET end_station_name = (
#         SELECT main_name
#         FROM (
#             SELECT
#                 end_station_id,
#                 CASE
#                     WHEN COUNT(DISTINCT end_station_name) = 1 THEN MAX(end_station_name)
#                     WHEN COUNT(DISTINCT end_station_name) > 1 THEN MAX(end_station_name)
#                 END AS main_name
#             FROM t_data
#             GROUP BY end_station_id
#         ) AS MainName
#         WHERE t_data.end_station_id = MainName.end_station_id
#     )
#     WHERE end_station_name != (
#         SELECT main_name
#         FROM (
#             SELECT
#                 end_station_id,
#                 CASE
#                     WHEN COUNT(DISTINCT end_station_name) = 1 THEN MAX(end_station_name)
#                     WHEN COUNT(DISTINCT end_station_name) > 1 THEN MAX(end_station_name)
#                 END AS main_name
#             FROM t_data
#             GROUP BY end_station_id
#         ) AS MainName
#         WHERE t_data.end_station_id = MainName.end_station_id
#     )") # 135099 issues
##################################################################
# NOTE: I changed the public rack data to most popular one too
# because seem the data is not relevant for the analysis i am planning
# to do.