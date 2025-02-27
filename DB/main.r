# ## Introduction

# This study is a part of Google Data Analytics Professional Certificate. The goal is to analyze the Divvy bike-share program following the six steps of the data analysis process: ask, prepare, process, analyze, share, and act. 
# The study will take from january of 2024 to january of 2025 as the time frame to analyze the data.

# ## Prepare

# ### Data Sources

# The data used in this study is from the Divvy bike-share program in Chicago. The data is available in CSV format and can be downloaded from the following links: [Datasets](https://divvy-tripdata.s3.amazonaws.com/index.html). 
# The data is divided into one file for each month and includes information about the start and end time of the trip, the start and end stations, the duration of the trip, and the user type.
# I will use SQL to merge all data, so will be easier to analyze the data. All file will be downloaded and saved in the same folder as the script.

# Load libraries
library(tidyverse)
library(DBI)
library(RMySQL)

# Push all CSV into mySQL database
# Connect to mySQL
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      dbname = "divvy",
                      host = "localhost",
                      username = "db_google_learn",
                      password = "123456")

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load all CSV files into mySQL that are in the same folder as the script and with the format 20*-divvy-tripdata
files <- list.files(pattern = "202[4-5]{1}[0-1]{1}[0-9]{1}-divvy-tripdata.csv")
for (file in files) {
  # Extract table name from file name
  table_name <- paste0("t_", gsub("-divvy-tripdata.csv", "", file))
  
  # Load CSV into mySQL
  dbWriteTable(con, table_name, read.csv(file), overwrite = TRUE)
}

# ## Process

# ### Data Wrangling

# Return ride_id duplicated
# print(dbGetQuery(con, "
#     SELECT 
#         ride_id 
#     FROM 
#         t_data 
#     GROUP BY 
#         ride_id 
#     HAVING 
#         COUNT(*) > 1
# "))



# delete duplicates from each table
dbExecute(con, "
    DELETE FROM t_202401
    WHERE ride_id IN (
            SELECT ride_id 
            FROM t_202401 
            GROUP BY ride_id 
            HAVING COUNT(*) > 1)")
dbExecute(con, "
    DELETE FROM 
        t_202402
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202402 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202403
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202403 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202404
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202404 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202405
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202405 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202406
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202406 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202407
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202407 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202408
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202408 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )   
")
dbExecute(con, "
    DELETE FROM 
        t_202409
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202409 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202410
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202410 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202411
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202411 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202412
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202412 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")
dbExecute(con, "
    DELETE FROM 
        t_202501
    WHERE 
        ride_id IN (
            SELECT 
                ride_id 
            FROM 
                t_202501 
            GROUP BY 
                ride_id 
            HAVING 
                COUNT(*) > 1
        )
")

# The data is now in a mySQL database. I will use SQL to wrangle the data, merging all tables into one and starting the data cleaning.

# Wrangle data
# Merge all tables into one
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

## Fixing Data Types
# Check if the type of columns are correct
print(dbGetQuery(con, "
    SELECT 
        COLUMN_NAME, 
        DATA_TYPE 
    FROM INFORMATION_SCHEMA.COLUMNS 
    WHERE TABLE NAME = 't_data'"))

# Fixing the columns type of started_at and ended_at to datetime
dbExecute(con, "
    ALTER TABLE t_data 
    MODIFY started_at DATETIME, 
    MODIFY ended_at DATETIME")

## Cleaning Data

# There are no duplicates in the data. I will now clean the data by removing missing values and outliers.

# delete duplicates from each table
dbExecute(con, "
    DELETE FROM t_data
    WHERE ride_id IN (
            SELECT ride_id 
            FROM t_data 
            GROUP BY ride_id 
            HAVING COUNT(*) > 1)")

dbExecute(con, "
    CREATE TEMPORARY TABLE temp_duplicados AS
    SELECT ride_id
    FROM t_data
    GROUP BY ride_id
    HAVING COUNT(*) > 1;

    DELETE FROM t_data
    WHERE ride_id IN (SELECT ride_id FROM temp_duplicados);

    DROP TEMPORARY TABLE temp_duplicados;")


# Remove missing values
dbExecute(con, "
    DELETE FROM t_data 
    WHERE 
        ride_id IS NULL OR
        rideable_type IS NULL OR
        started_at IS NULL OR 
        ended_at IS NULL OR 
        member_casual IS NULL")


# Remove outliers if the started_at is greater than ended_at or started_at - ended_at is greater than 24 hours or less than 1 minute

dbExecute(con, "
    DELETE FROM  t_data 
    WHERE 
        started_at > ended_at OR 
        TIMESTAMPDIFF(SECOND, started_at, ended_at) > 86400 OR 
        TIMESTAMPDIFF(SECOND, started_at, ended_at) < 60")
