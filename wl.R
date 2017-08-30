# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:45:31 x86_64-w64-mingw32 

# Rattle versiÃƒÆ’Ã‚Â³n 5.0.19 usuario 'Carlos'

# This log captures Rattle interactions as an R script. 

# For repeatability export this log of all activity to a 
# file using the Export button or the Tools menu. This 
# script can serve as a starting point for developing your 
# own scripts. Exporting to a file called 'model.R' will 
# allow you to type into a new R Console the command 
#"source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access weather dataset and utilities.
library(magrittr) # For the %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:46:03 x86_64-w64-mingw32 

# Load the dataset from file.

fname <- "file:///C:/Users/Carlos/OneDrive/DS/2017 Coursera PLM/pml-training.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:46:10 x86_64-w64-mingw32 

# Tome en cuenta las selecciones del usuario. 

# Build the train/validate/test datasets.

# nobs=19622 train=13735 validate=2943 test=2944

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$sample   <- crs$train <-sample(nrow(crs$dataset), 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate)

# Se han anotado las siguientes selecciones de variable.

crs$input     <- c("user_name", "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "cvtd_timestamp", "new_window",
                   "num_window", "roll_belt", "pitch_belt",
                   "yaw_belt", "total_accel_belt",
                   "kurtosis_roll_belt",
                   "kurtosis_picth_belt",
                   "skewness_roll_belt",
                   "skewness_roll_belt.1", "max_roll_belt",
                   "max_picth_belt", "max_yaw_belt",
                   "min_roll_belt", "min_pitch_belt",
                   "min_yaw_belt", "amplitude_roll_belt",
                   "amplitude_pitch_belt",
                   "amplitude_yaw_belt",
                   "var_total_accel_belt", "avg_roll_belt",
                   "stddev_roll_belt", "var_roll_belt",
                   "avg_pitch_belt", "stddev_pitch_belt",
                   "var_pitch_belt", "avg_yaw_belt",
                   "stddev_yaw_belt", "var_yaw_belt",
                   "gyros_belt_x", "gyros_belt_y",
                   "gyros_belt_z", "accel_belt_x",
                   "accel_belt_y", "accel_belt_z",
                   "magnet_belt_x", "magnet_belt_y",
                   "magnet_belt_z", "roll_arm",
                   "pitch_arm", "yaw_arm",
                   "total_accel_arm", "var_accel_arm",
                   "avg_roll_arm", "stddev_roll_arm",
                   "var_roll_arm", "avg_pitch_arm",
                   "stddev_pitch_arm", "var_pitch_arm",
                   "avg_yaw_arm", "stddev_yaw_arm",
                   "var_yaw_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "kurtosis_roll_arm",
                   "kurtosis_picth_arm",
                   "kurtosis_yaw_arm", "skewness_roll_arm",
                   "skewness_pitch_arm",
                   "skewness_yaw_arm", "max_roll_arm",
                   "max_picth_arm", "max_yaw_arm",
                   "min_roll_arm", "min_pitch_arm",
                   "min_yaw_arm", "amplitude_roll_arm",
                   "amplitude_pitch_arm",
                   "amplitude_yaw_arm", "roll_dumbbell",
                   "pitch_dumbbell", "yaw_dumbbell",
                   "kurtosis_roll_dumbbell",
                   "kurtosis_picth_dumbbell",
                   "skewness_roll_dumbbell",
                   "skewness_pitch_dumbbell",
                   "max_roll_dumbbell",
                   "max_picth_dumbbell",
                   "max_yaw_dumbbell", "min_roll_dumbbell",
                   "min_pitch_dumbbell",
                   "min_yaw_dumbbell",
                   "amplitude_roll_dumbbell",
                   "amplitude_pitch_dumbbell",
                   "amplitude_yaw_dumbbell",
                   "total_accel_dumbbell",
                   "var_accel_dumbbell",
                   "avg_roll_dumbbell",
                   "stddev_roll_dumbbell",
                   "var_roll_dumbbell",
                   "avg_pitch_dumbbell",
                   "stddev_pitch_dumbbell",
                   "var_pitch_dumbbell",
                   "avg_yaw_dumbbell",
                   "stddev_yaw_dumbbell",
                   "var_yaw_dumbbell", "gyros_dumbbell_x",
                   "gyros_dumbbell_y", "gyros_dumbbell_z",
                   "accel_dumbbell_x", "accel_dumbbell_y",
                   "accel_dumbbell_z", "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "kurtosis_roll_forearm",
                   "kurtosis_picth_forearm",
                   "skewness_roll_forearm",
                   "skewness_pitch_forearm",
                   "max_roll_forearm", "max_picth_forearm",
                   "max_yaw_forearm", "min_roll_forearm",
                   "min_pitch_forearm", "min_yaw_forearm",
                   "amplitude_roll_forearm",
                   "amplitude_pitch_forearm",
                   "amplitude_yaw_forearm",
                   "total_accel_forearm",
                   "var_accel_forearm", "avg_roll_forearm",
                   "stddev_roll_forearm",
                   "var_roll_forearm", "avg_pitch_forearm",
                   "stddev_pitch_forearm",
                   "var_pitch_forearm", "avg_yaw_forearm",
                   "stddev_yaw_forearm", "var_yaw_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$numeric   <- c("raw_timestamp_part_1",
                   "raw_timestamp_part_2", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "max_roll_belt",
                   "max_picth_belt", "min_roll_belt",
                   "min_pitch_belt", "amplitude_roll_belt",
                   "amplitude_pitch_belt",
                   "var_total_accel_belt", "avg_roll_belt",
                   "stddev_roll_belt", "var_roll_belt",
                   "avg_pitch_belt", "stddev_pitch_belt",
                   "var_pitch_belt", "avg_yaw_belt",
                   "stddev_yaw_belt", "var_yaw_belt",
                   "gyros_belt_x", "gyros_belt_y",
                   "gyros_belt_z", "accel_belt_x",
                   "accel_belt_y", "accel_belt_z",
                   "magnet_belt_x", "magnet_belt_y",
                   "magnet_belt_z", "roll_arm",
                   "pitch_arm", "yaw_arm",
                   "total_accel_arm", "var_accel_arm",
                   "avg_roll_arm", "stddev_roll_arm",
                   "var_roll_arm", "avg_pitch_arm",
                   "stddev_pitch_arm", "var_pitch_arm",
                   "avg_yaw_arm", "stddev_yaw_arm",
                   "var_yaw_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "max_roll_arm", "max_picth_arm",
                   "max_yaw_arm", "min_roll_arm",
                   "min_pitch_arm", "min_yaw_arm",
                   "amplitude_roll_arm",
                   "amplitude_pitch_arm",
                   "amplitude_yaw_arm", "roll_dumbbell",
                   "pitch_dumbbell", "yaw_dumbbell",
                   "max_roll_dumbbell",
                   "max_picth_dumbbell",
                   "min_roll_dumbbell",
                   "min_pitch_dumbbell",
                   "amplitude_roll_dumbbell",
                   "amplitude_pitch_dumbbell",
                   "total_accel_dumbbell",
                   "var_accel_dumbbell",
                   "avg_roll_dumbbell",
                   "stddev_roll_dumbbell",
                   "var_roll_dumbbell",
                   "avg_pitch_dumbbell",
                   "stddev_pitch_dumbbell",
                   "var_pitch_dumbbell",
                   "avg_yaw_dumbbell",
                   "stddev_yaw_dumbbell",
                   "var_yaw_dumbbell", "gyros_dumbbell_x",
                   "gyros_dumbbell_y", "gyros_dumbbell_z",
                   "accel_dumbbell_x", "accel_dumbbell_y",
                   "accel_dumbbell_z", "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "max_roll_forearm", "max_picth_forearm",
                   "min_roll_forearm", "min_pitch_forearm",
                   "amplitude_roll_forearm",
                   "amplitude_pitch_forearm",
                   "total_accel_forearm",
                   "var_accel_forearm", "avg_roll_forearm",
                   "stddev_roll_forearm",
                   "var_roll_forearm", "avg_pitch_forearm",
                   "stddev_pitch_forearm",
                   "var_pitch_forearm", "avg_yaw_forearm",
                   "stddev_yaw_forearm", "var_yaw_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$categoric <- c("user_name", "cvtd_timestamp",
                   "new_window", "kurtosis_roll_belt",
                   "kurtosis_picth_belt",
                   "skewness_roll_belt",
                   "skewness_roll_belt.1", "max_yaw_belt",
                   "min_yaw_belt", "amplitude_yaw_belt",
                   "kurtosis_roll_arm",
                   "kurtosis_picth_arm",
                   "kurtosis_yaw_arm", "skewness_roll_arm",
                   "skewness_pitch_arm",
                   "skewness_yaw_arm",
                   "kurtosis_roll_dumbbell",
                   "kurtosis_picth_dumbbell",
                   "skewness_roll_dumbbell",
                   "skewness_pitch_dumbbell",
                   "max_yaw_dumbbell", "min_yaw_dumbbell",
                   "amplitude_yaw_dumbbell",
                   "kurtosis_roll_forearm",
                   "kurtosis_picth_forearm",
                   "skewness_roll_forearm",
                   "skewness_pitch_forearm",
                   "max_yaw_forearm", "min_yaw_forearm",
                   "amplitude_yaw_forearm")

crs$target    <- "classe"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- c("kurtosis_yaw_belt", "skewness_yaw_belt", "kurtosis_yaw_dumbbell", "skewness_yaw_dumbbell", "kurtosis_yaw_forearm", "skewness_yaw_forearm")
crs$weights   <- NULL

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:46:31 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funciÃƒÆ’Ã‚Â³n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:46:59 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funciÃƒÆ’Ã‚Â³n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:48:00 x86_64-w64-mingw32 

# Tome en cuenta las selecciones del usuario. 

# Build the train/validate/test datasets.

# nobs=19622 train=13735 validate=2943 test=2944

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$sample   <- crs$train <-sample(nrow(crs$dataset), 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate)

# Se han anotado las siguientes selecciones de variable.

crs$input     <- c("user_name", "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "cvtd_timestamp", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt",
                   "kurtosis_roll_belt",
                   "kurtosis_picth_belt",
                   "skewness_roll_belt",
                   "skewness_roll_belt.1", "max_roll_belt",
                   "max_picth_belt", "max_yaw_belt",
                   "min_roll_belt", "min_pitch_belt",
                   "min_yaw_belt", "amplitude_roll_belt",
                   "amplitude_pitch_belt",
                   "amplitude_yaw_belt",
                   "var_total_accel_belt", "avg_roll_belt",
                   "stddev_roll_belt", "var_roll_belt",
                   "avg_pitch_belt", "stddev_pitch_belt",
                   "var_pitch_belt", "avg_yaw_belt",
                   "stddev_yaw_belt", "var_yaw_belt",
                   "gyros_belt_x", "gyros_belt_y",
                   "gyros_belt_z", "accel_belt_x",
                   "accel_belt_y", "accel_belt_z",
                   "magnet_belt_x", "magnet_belt_y",
                   "magnet_belt_z", "roll_arm",
                   "pitch_arm", "yaw_arm",
                   "total_accel_arm", "var_accel_arm",
                   "avg_roll_arm", "stddev_roll_arm",
                   "var_roll_arm", "avg_pitch_arm",
                   "stddev_pitch_arm", "var_pitch_arm",
                   "avg_yaw_arm", "stddev_yaw_arm",
                   "var_yaw_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "kurtosis_roll_arm",
                   "kurtosis_picth_arm",
                   "kurtosis_yaw_arm", "skewness_roll_arm",
                   "skewness_pitch_arm",
                   "skewness_yaw_arm", "max_roll_arm",
                   "max_picth_arm", "max_yaw_arm",
                   "min_roll_arm", "min_pitch_arm",
                   "min_yaw_arm", "amplitude_roll_arm",
                   "amplitude_pitch_arm",
                   "amplitude_yaw_arm", "roll_dumbbell",
                   "pitch_dumbbell", "yaw_dumbbell",
                   "kurtosis_roll_dumbbell",
                   "kurtosis_picth_dumbbell",
                   "skewness_roll_dumbbell",
                   "skewness_pitch_dumbbell",
                   "max_roll_dumbbell",
                   "max_picth_dumbbell",
                   "max_yaw_dumbbell", "min_roll_dumbbell",
                   "min_pitch_dumbbell",
                   "min_yaw_dumbbell",
                   "amplitude_roll_dumbbell",
                   "amplitude_pitch_dumbbell",
                   "amplitude_yaw_dumbbell",
                   "total_accel_dumbbell",
                   "var_accel_dumbbell",
                   "avg_roll_dumbbell",
                   "stddev_roll_dumbbell",
                   "var_roll_dumbbell",
                   "avg_pitch_dumbbell",
                   "stddev_pitch_dumbbell",
                   "var_pitch_dumbbell",
                   "avg_yaw_dumbbell",
                   "stddev_yaw_dumbbell",
                   "var_yaw_dumbbell", "gyros_dumbbell_x",
                   "gyros_dumbbell_y", "gyros_dumbbell_z",
                   "accel_dumbbell_x", "accel_dumbbell_y",
                   "accel_dumbbell_z", "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "kurtosis_roll_forearm",
                   "kurtosis_picth_forearm",
                   "skewness_roll_forearm",
                   "skewness_pitch_forearm",
                   "max_roll_forearm", "max_picth_forearm",
                   "max_yaw_forearm", "min_roll_forearm",
                   "min_pitch_forearm", "min_yaw_forearm",
                   "amplitude_roll_forearm",
                   "amplitude_pitch_forearm",
                   "amplitude_yaw_forearm",
                   "total_accel_forearm",
                   "var_accel_forearm", "avg_roll_forearm",
                   "stddev_roll_forearm",
                   "var_roll_forearm", "avg_pitch_forearm",
                   "stddev_pitch_forearm",
                   "var_pitch_forearm", "avg_yaw_forearm",
                   "stddev_yaw_forearm", "var_yaw_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$numeric   <- c("raw_timestamp_part_1",
                   "raw_timestamp_part_2", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "max_roll_belt",
                   "max_picth_belt", "min_roll_belt",
                   "min_pitch_belt", "amplitude_roll_belt",
                   "amplitude_pitch_belt",
                   "var_total_accel_belt", "avg_roll_belt",
                   "stddev_roll_belt", "var_roll_belt",
                   "avg_pitch_belt", "stddev_pitch_belt",
                   "var_pitch_belt", "avg_yaw_belt",
                   "stddev_yaw_belt", "var_yaw_belt",
                   "gyros_belt_x", "gyros_belt_y",
                   "gyros_belt_z", "accel_belt_x",
                   "accel_belt_y", "accel_belt_z",
                   "magnet_belt_x", "magnet_belt_y",
                   "magnet_belt_z", "roll_arm",
                   "pitch_arm", "yaw_arm",
                   "total_accel_arm", "var_accel_arm",
                   "avg_roll_arm", "stddev_roll_arm",
                   "var_roll_arm", "avg_pitch_arm",
                   "stddev_pitch_arm", "var_pitch_arm",
                   "avg_yaw_arm", "stddev_yaw_arm",
                   "var_yaw_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "max_roll_arm", "max_picth_arm",
                   "max_yaw_arm", "min_roll_arm",
                   "min_pitch_arm", "min_yaw_arm",
                   "amplitude_roll_arm",
                   "amplitude_pitch_arm",
                   "amplitude_yaw_arm", "roll_dumbbell",
                   "pitch_dumbbell", "yaw_dumbbell",
                   "max_roll_dumbbell",
                   "max_picth_dumbbell",
                   "min_roll_dumbbell",
                   "min_pitch_dumbbell",
                   "amplitude_roll_dumbbell",
                   "amplitude_pitch_dumbbell",
                   "total_accel_dumbbell",
                   "var_accel_dumbbell",
                   "avg_roll_dumbbell",
                   "stddev_roll_dumbbell",
                   "var_roll_dumbbell",
                   "avg_pitch_dumbbell",
                   "stddev_pitch_dumbbell",
                   "var_pitch_dumbbell",
                   "avg_yaw_dumbbell",
                   "stddev_yaw_dumbbell",
                   "var_yaw_dumbbell", "gyros_dumbbell_x",
                   "gyros_dumbbell_y", "gyros_dumbbell_z",
                   "accel_dumbbell_x", "accel_dumbbell_y",
                   "accel_dumbbell_z", "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "max_roll_forearm", "max_picth_forearm",
                   "min_roll_forearm", "min_pitch_forearm",
                   "amplitude_roll_forearm",
                   "amplitude_pitch_forearm",
                   "total_accel_forearm",
                   "var_accel_forearm", "avg_roll_forearm",
                   "stddev_roll_forearm",
                   "var_roll_forearm", "avg_pitch_forearm",
                   "stddev_pitch_forearm",
                   "var_pitch_forearm", "avg_yaw_forearm",
                   "stddev_yaw_forearm", "var_yaw_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$categoric <- c("user_name", "cvtd_timestamp",
                   "kurtosis_roll_belt",
                   "kurtosis_picth_belt",
                   "skewness_roll_belt",
                   "skewness_roll_belt.1", "max_yaw_belt",
                   "min_yaw_belt", "amplitude_yaw_belt",
                   "kurtosis_roll_arm",
                   "kurtosis_picth_arm",
                   "kurtosis_yaw_arm", "skewness_roll_arm",
                   "skewness_pitch_arm",
                   "skewness_yaw_arm",
                   "kurtosis_roll_dumbbell",
                   "kurtosis_picth_dumbbell",
                   "skewness_roll_dumbbell",
                   "skewness_pitch_dumbbell",
                   "max_yaw_dumbbell", "min_yaw_dumbbell",
                   "amplitude_yaw_dumbbell",
                   "kurtosis_roll_forearm",
                   "kurtosis_picth_forearm",
                   "skewness_roll_forearm",
                   "skewness_pitch_forearm",
                   "max_yaw_forearm", "min_yaw_forearm",
                   "amplitude_yaw_forearm")

crs$target    <- "classe"
crs$risk      <- NULL
crs$ident     <- c("X", "new_window")
crs$ignore    <- c("kurtosis_yaw_belt", "skewness_yaw_belt", "kurtosis_yaw_dumbbell", "skewness_yaw_dumbbell", "kurtosis_yaw_forearm", "skewness_yaw_forearm")
crs$weights   <- NULL

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:48:04 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funciÃƒÆ’Ã‚Â³n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:49:11 x86_64-w64-mingw32 

# MÃƒÆ’Ã‚Â¡quina de vector de apoyo 

# El paquete 'kernlab' ofrece la funciÃƒÆ’Ã‚Â³n 'ksvm'.

library(kernlab, quietly=TRUE)

# Construir un modelo de mÃƒÆ’Ã‚Â¡quina de vector de apoyo.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(classe) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="vanilladot",
      prob.model=TRUE)

# Generar una vista textual del modelo SVM.

crs$ksvm

# Tiempo transcurrido: 21.95 segs

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:49:47 x86_64-w64-mingw32 

# AnÃƒÆ’Ã‚Â¡lisis de componentes principales (solo numÃƒÆ’Ã‚Â©ricos).

pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Mostrar la salida del anÃƒÆ’Ã‚Â¡lisis.

pc

# Resumir la importancia de los componentes encontrados.

summary(pc)

# Desplegar un diagrama que muestra la importancia relativa de los componentes.

plot(pc, main="")
title(main="Importancia de los componentes principales pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Desplegar un diagrama que muestra los dos componentes principales.

biplot(pc, main="")
title(main="Componentes principales pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:50:39 x86_64-w64-mingw32 

# Generar un diagrama de correlaciÃƒÆ’Ã‚Â³n para las variables. 

# El paquete 'corrplot' ofrece la funciÃƒÆ’Ã‚Â³n 'corrplot'.

library(corrplot, quietly=TRUE)

# Las correlaciones solo funcionan para variables numÃƒÆ’Ã‚Â©ricas.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Organice las correlaciones por su fortaleza.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Mostrar las correlaciones reales.

print(crs$cor)

# Mostrar grÃƒÆ’Ã‚Â¡ficamente las correlaciones.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="CorrelaciÃƒÆ’Ã‚Â³n pml-training.csv usando Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:51:16 x86_64-w64-mingw32 

# AnÃƒÆ’Ã‚Â¡lisis de componentes principales (solo numÃƒÆ’Ã‚Â©ricos).

pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Mostrar la salida del anÃƒÆ’Ã‚Â¡lisis.

pc

# Resumir la importancia de los componentes encontrados.

summary(pc)

# Desplegar un diagrama que muestra la importancia relativa de los componentes.

plot(pc, main="")
title(main="Importancia de los componentes principales pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Desplegar un diagrama que muestra los dos componentes principales.

biplot(pc, main="")
title(main="Componentes principales pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:51:37 x86_64-w64-mingw32 

# El paquete 'Hmisc' ofrece la funciÃƒÆ’Ã‚Â³n 'contents'.

library(Hmisc, quietly=TRUE)

# Obtener un resumen del conjunto de datos.

contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:52:34 x86_64-w64-mingw32 

# KMeans 

# Reajustar la semilla de nÃƒÆ’Ã‚Âºmero aleatorio para obtener los mismos resultados cada vez.

set.seed(crv$seed)

# El paquete 'reshape' ofrece la funciÃƒÆ’Ã‚Â³n 'rescaler'.

library(reshape, quietly=TRUE)

# Generar un clÃƒÆ’Ã‚Âºster kmeans con tamaÃƒÆ’Ã‚Â±o de 10.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"), 10)

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:52:40 x86_64-w64-mingw32 

# Informar sobre las caracterÃƒÆ’Ã‚Â­sticas de clÃƒÆ’Ã‚Âºster. 

# TamaÃƒÆ’Ã‚Â±os de clÃƒÆ’Ã‚Âºsters:

paste(crs$kmeans$size, collapse=' ')

# Medias de datos:

colMeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"))

# Centros de clÃƒÆ’Ã‚Âºsters:

crs$kmeans$centers

# Suma de cuadrados en clÃƒÆ’Ã‚Âºster:

crs$kmeans$withinss

# Tiempo transcurrido: 0.10 segs

# Generar un diagrama de coordenadas discriminantes.

cluster::clusplot(na.omit(crs$dataset[crs$sample, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates pml-training.csv')


#============================================================
# Rattle marca de tiempo: 2017-08-26 20:53:19 x86_64-w64-mingw32 

# Generar estadÃƒÆ’Ã‚Â­sticas de clÃƒÆ’Ã‚Âºster. 

# El paquete 'fpc' ofrece la funciÃƒÆ’Ã‚Â³n 'cluster.stats'.

library(fpc, quietly=TRUE)

# El paquete 'fpc' ofrece la funciÃƒÆ’Ã‚Â³n 'cluster.stats'.

cluster.stats(dist(na.omit(crs$dataset[crs$sample, crs$numeric])), crs$kmeans$cluster)


#============================================================
# Rattle marca de tiempo: 2017-08-26 20:53:24 x86_64-w64-mingw32 

# Desplegar una matriz de diagrama de dispersiÃƒÆ’Ã‚Â³n para el clÃƒÆ’Ã‚Âºster KMeans. 

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:53:53 x86_64-w64-mingw32 

# Score the validation dataset. 

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:54:13 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generar una matriz de error para el modelo SVM.

# Obtenga la respuesta del modelo SVM.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:54:50 x86_64-w64-mingw32 

# Modelo de regresiÃƒÆ’Ã‚Â³n 

# Construir un modelo polinÃƒÆ’Ã‚Â³mico usando el paquete nnet.

library(nnet, quietly=TRUE)

# Summarise multinomial model using Anova from the car package.

library(car, quietly=TRUE)

# Construir un modelo de regresiÃƒÆ’Ã‚Â³n.

crs$glm <- multinom(classe ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], trace=FALSE, maxit=1000)

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:55:21 x86_64-w64-mingw32 

# Modelo de regresiÃƒÆ’Ã‚Â³n 

# Construir un modelo polinÃƒÆ’Ã‚Â³mico usando el paquete nnet.

library(nnet, quietly=TRUE)

# Summarise multinomial model using Anova from the car package.

library(car, quietly=TRUE)

# Construir un modelo de regresiÃƒÆ’Ã‚Â³n.

crs$glm <- multinom(classe ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], trace=FALSE, maxit=1000)

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:55:35 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generar una matriz de error para el modelo SVM.

# Obtenga la respuesta del modelo SVM.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:56:05 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generar una matriz de error para el modelo SVM.

# Obtenga la respuesta del modelo SVM.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generar un diagrama de coordenadas discriminantes.

cluster::clusplot(na.omit(crs$dataset[crs$sample, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates pml-training.csv')


#============================================================
# Rattle marca de tiempo: 2017-08-26 20:57:05 x86_64-w64-mingw32 

# Desplegar una matriz de diagrama de dispersiÃƒÆ’Ã‚Â³n para el clÃƒÆ’Ã‚Âºster KMeans. 

# Mantener solo las primeras variables 5 para el diagrama.

vars <- 1:5

# Generar un diagrama de datos.

plot(na.omit(crs$dataset[crs$sample, intersect(crs$input, crs$numeric)][vars]), col=crs$kmeans$cluster)
title(main="",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:59:13 x86_64-w64-mingw32 

# Perform Test 

# Use el paquete fBasics para pruebas estadÃƒÆ’Ã‚Â­sticas.

library(fBasics, quietly=TRUE)

# Realizar la prueba.

correlationTest(na.omit(crs$dataset[, "pitch_belt"]), na.omit(crs$dataset[, "raw_timestamp_part_2"]))

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:59:29 x86_64-w64-mingw32 

# El paquete 'Hmisc' ofrece la funciÃƒÆ’Ã‚Â³n 'contents'.

library(Hmisc, quietly=TRUE)

# Obtener un resumen del conjunto de datos.

contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# El paquete 'Hmisc' ofrece la funciÃƒÆ’Ã‚Â³n 'describe'.

library(Hmisc, quietly=TRUE)

# Generar una descripciÃƒÆ’Ã‚Â³n del conjunto de datos.

describe(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:59:35 x86_64-w64-mingw32 

# El paquete 'Hmisc' ofrece la funciÃƒÆ’Ã‚Â³n 'contents'.

library(Hmisc, quietly=TRUE)

# Obtener un resumen del conjunto de datos.

contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# El paquete 'Hmisc' ofrece la funciÃƒÆ’Ã‚Â³n 'describe'.

library(Hmisc, quietly=TRUE)

# Generar una descripciÃƒÆ’Ã‚Â³n del conjunto de datos.

describe(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

#============================================================
# Rattle marca de tiempo: 2017-08-26 20:59:38 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funciÃƒÆ’Ã‚Â³n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:44:41 x86_64-w64-mingw32 

# Recargar los datos del proyecto (crs variables) desde el archivo.

load("C:\Users\Carlos\OneDrive\DS\2017 Coursera PLM\WL.rattle")

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:48:26 x86_64-w64-mingw32 

# Tome en cuenta las selecciones del usuario. 

# Build the train/validate/test datasets.

# nobs=19622 train=13735 validate=2943 test=2944

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$sample   <- crs$train <-sample(nrow(crs$dataset), 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate)

# Se han anotado las siguientes selecciones de variable.

crs$input     <- c("user_name", "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "cvtd_timestamp", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$numeric   <- c("raw_timestamp_part_1",
                   "raw_timestamp_part_2", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$categoric <- c("user_name", "cvtd_timestamp")

crs$target    <- "classe"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", "max_roll_belt", "max_picth_belt", "max_yaw_belt", "min_roll_belt", "min_pitch_belt", "min_yaw_belt", "amplitude_roll_belt", "amplitude_pitch_belt", "amplitude_yaw_belt", "var_total_accel_belt", "avg_roll_belt", "stddev_roll_belt", "var_roll_belt", "avg_pitch_belt", "stddev_pitch_belt", "var_pitch_belt", "avg_yaw_belt", "stddev_yaw_belt", "var_yaw_belt", "var_accel_arm", "avg_roll_arm", "stddev_roll_arm", "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm", "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm", "max_roll_arm", "max_picth_arm", "max_yaw_arm", "min_roll_arm", "min_pitch_arm", "min_yaw_arm", "amplitude_roll_arm", "amplitude_pitch_arm", "amplitude_yaw_arm", "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell", "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_roll_dumbbell", "max_picth_dumbbell", "max_yaw_dumbbell", "min_roll_dumbbell", "min_pitch_dumbbell", "min_yaw_dumbbell", "amplitude_roll_dumbbell", "amplitude_pitch_dumbbell", "amplitude_yaw_dumbbell", "var_accel_dumbbell", "avg_roll_dumbbell", "stddev_roll_dumbbell", "var_roll_dumbbell", "avg_pitch_dumbbell", "stddev_pitch_dumbbell", "var_pitch_dumbbell", "avg_yaw_dumbbell", "stddev_yaw_dumbbell", "var_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm", "max_picth_forearm", "max_yaw_forearm", "min_roll_forearm", "min_pitch_forearm", "min_yaw_forearm", "amplitude_roll_forearm", "amplitude_pitch_forearm", "amplitude_yaw_forearm", "var_accel_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm", "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm", "stddev_yaw_forearm", "var_yaw_forearm")
crs$weights   <- NULL

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:48:29 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funciÃƒÂ³n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:49:34 x86_64-w64-mingw32 

# Build a Bosque aleatorio model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(classe ~ .,
  data=crs$dataset[crs$sample, c(crs$input, crs$target)], 
  ntree=500,
  mtry=100,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Bosque aleatorio' model.

crs$rf

# Enumerar la importancia de las variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Tiempo transcurrido: 2.90 mins

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:53:01 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:53:33 x86_64-w64-mingw32 

# Diagramar la importancia relativa de las variables.

p <- ggVarImp(crs$rf,
              title="Importancia de variable Bosque aleatorio pml-training.csv")
p

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:54:05 x86_64-w64-mingw32 

# Generar un diagrama de correlaciÃƒÂ³n para las variables. 

# El paquete 'corrplot' ofrece la funciÃƒÂ³n 'corrplot'.

library(corrplot, quietly=TRUE)

# Las correlaciones solo funcionan para variables numÃƒÂ©ricas.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Organice las correlaciones por su fortaleza.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Mostrar las correlaciones reales.

print(crs$cor)

# Mostrar grÃƒÂ¡ficamente las correlaciones.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="CorrelaciÃƒÂ³n pml-training.csv usando Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:54:26 x86_64-w64-mingw32 

# AnÃƒÂ¡lisis de componentes principales (solo numÃƒÂ©ricos).

pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Mostrar la salida del anÃƒÂ¡lisis.

pc

# Resumir la importancia de los componentes encontrados.

summary(pc)

# Desplegar un diagrama que muestra la importancia relativa de los componentes.

plot(pc, main="")
title(main="Importancia de los componentes principales pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Desplegar un diagrama que muestra los dos componentes principales.

biplot(pc, main="")
title(main="Componentes principales pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve.

library(verification)
aucc <- verification::roc.area(as.integer(as.factor(na.omit(crs$dataset[crs$sample,])[, crs$target]))-1,
                 crs$rf$votes[,2])$A
verification::roc.plot(as.integer(as.factor(na.omit(crs$dataset[crs$sample,])[, crs$target]))-1,
         crs$rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Bosque aleatorio pml-training.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:55:59 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# Leer un conjunto de datos de un archivo para probar el modelo.

crs$testset <- read.csv("C:/Users/Carlos/OneDrive/DS/2017 Coursera PLM/pml-testing.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 13:58:37 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:00:06 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:00:15 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:00:40 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃƒÂºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:02:13 x86_64-w64-mingw32 

# Recargar los datos del proyecto (crs variables) desde el archivo.

load("C:\Users\Carlos\OneDrive\DS\2017 Coursera PLM\WL.rattle")

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:02:31 x86_64-w64-mingw32 

# MÃ¡quina de vector de apoyo 

# El paquete 'kernlab' ofrece la funciÃ³n 'ksvm'.

library(kernlab, quietly=TRUE)

# Construir un modelo de mÃ¡quina de vector de apoyo.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(classe) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="vanilladot",
      prob.model=TRUE)

# Generar una vista textual del modelo SVM.

crs$ksvm

# Tiempo transcurrido: 47.10 segs

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:03:42 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo SVM.

# Obtenga la respuesta del modelo SVM.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:03:55 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo SVM.

# Obtenga la respuesta del modelo SVM.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:04:06 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:04:31 x86_64-w64-mingw32 

# Guarde los datos del proyecto (crs variables) en un archivo.

save(crs, file="C:/Users\Carlos\OneDrive\DS\2017 Coursera PLM\WL.rattle", compress=TRUE)

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:05:02 x86_64-w64-mingw32 

# Generar un diagrama de correlaciÃ³n para las variables. 

# El paquete 'corrplot' ofrece la funciÃ³n 'corrplot'.

library(corrplot, quietly=TRUE)

# Las correlaciones solo funcionan para variables numÃ©ricas.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Organice las correlaciones por su fortaleza.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Mostrar las correlaciones reales.

print(crs$cor)

# Mostrar grÃ¡ficamente las correlaciones.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="CorrelaciÃ³n pml-training.csv usando Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:05:43 x86_64-w64-mingw32 

# Build a Bosque aleatorio model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(classe ~ .,
  data=crs$dataset[crs$sample, c(crs$input, crs$target)], 
  ntree=500,
  mtry=100,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Bosque aleatorio' model.

crs$rf

# Enumerar la importancia de las variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Tiempo transcurrido: 2.88 mins

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:31:41 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:39:39 x86_64-w64-mingw32 

# Tome en cuenta las selecciones del usuario. 

# Build the train/validate/test datasets.

# nobs=19622 train=13735 validate=2943 test=2944

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$sample   <- crs$train <-sample(nrow(crs$dataset), 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate)

# Se han anotado las siguientes selecciones de variable.

crs$input     <- c("user_name", "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "cvtd_timestamp", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$numeric   <- c("raw_timestamp_part_1",
                   "raw_timestamp_part_2", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$categoric <- c("user_name", "cvtd_timestamp")

crs$target    <- "classe"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", "max_roll_belt", "max_picth_belt", "max_yaw_belt", "min_roll_belt", "min_pitch_belt", "min_yaw_belt", "amplitude_roll_belt", "amplitude_pitch_belt", "amplitude_yaw_belt", "var_total_accel_belt", "avg_roll_belt", "stddev_roll_belt", "var_roll_belt", "avg_pitch_belt", "stddev_pitch_belt", "var_pitch_belt", "avg_yaw_belt", "stddev_yaw_belt", "var_yaw_belt", "var_accel_arm", "avg_roll_arm", "stddev_roll_arm", "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm", "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm", "max_roll_arm", "max_picth_arm", "max_yaw_arm", "min_roll_arm", "min_pitch_arm", "min_yaw_arm", "amplitude_roll_arm", "amplitude_pitch_arm", "amplitude_yaw_arm", "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell", "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_roll_dumbbell", "max_picth_dumbbell", "max_yaw_dumbbell", "min_roll_dumbbell", "min_pitch_dumbbell", "min_yaw_dumbbell", "amplitude_roll_dumbbell", "amplitude_pitch_dumbbell", "amplitude_yaw_dumbbell", "var_accel_dumbbell", "avg_roll_dumbbell", "stddev_roll_dumbbell", "var_roll_dumbbell", "avg_pitch_dumbbell", "stddev_pitch_dumbbell", "var_pitch_dumbbell", "avg_yaw_dumbbell", "stddev_yaw_dumbbell", "var_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm", "max_picth_forearm", "max_yaw_forearm", "min_roll_forearm", "min_pitch_forearm", "min_yaw_forearm", "amplitude_roll_forearm", "amplitude_pitch_forearm", "amplitude_yaw_forearm", "var_accel_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm", "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm", "stddev_yaw_forearm", "var_yaw_forearm")
crs$weights   <- NULL

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:39:43 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funciÃ³n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:42:16 x86_64-w64-mingw32 

# Build a Bosque aleatorio model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(classe ~ .,
  data=crs$dataset[crs$sample, c(crs$input, crs$target)], 
  ntree=500,
  mtry=100,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Bosque aleatorio' model.

crs$rf

# Enumerar la importancia de las variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Tiempo transcurrido: 2.88 mins

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:45:29 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:45:44 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 14:46:09 x86_64-w64-mingw32 

# Score the testing dataset. 

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# AsegÃºrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Obtenga calificaciones de probabilidad para el modelo Bosque aleatorio en pml-training.csv [prueba].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input)]))

# Extraer la variables relevantes del conjunto de datos.

sdata <- crs$dataset[crs$test,]

# Sacar los datos combinados.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Carlos\Desktop\pml-training_prueba_score_all.csv", row.names=FALSE)

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:10:11 x86_64-w64-mingw32 

# Recargar los datos del proyecto (crs variables) desde el archivo.

load("C:\Users\Carlos\OneDrive\DS\2017 Coursera PLM\WL.rattle")

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:10:34 x86_64-w64-mingw32 

# Tome en cuenta las selecciones del usuario. 

# Build the train/validate/test datasets.

# nobs=19622 train=13735 validate=2943 test=2944

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$sample   <- crs$train <-sample(nrow(crs$dataset), 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate)

# Se han anotado las siguientes selecciones de variable.

crs$input     <- c("user_name", "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "cvtd_timestamp", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$numeric   <- c("raw_timestamp_part_1",
                   "raw_timestamp_part_2", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$categoric <- c("user_name", "cvtd_timestamp")

crs$target    <- "classe"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", "max_roll_belt", "max_picth_belt", "max_yaw_belt", "min_roll_belt", "min_pitch_belt", "min_yaw_belt", "amplitude_roll_belt", "amplitude_pitch_belt", "amplitude_yaw_belt", "var_total_accel_belt", "avg_roll_belt", "stddev_roll_belt", "var_roll_belt", "avg_pitch_belt", "stddev_pitch_belt", "var_pitch_belt", "avg_yaw_belt", "stddev_yaw_belt", "var_yaw_belt", "var_accel_arm", "avg_roll_arm", "stddev_roll_arm", "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm", "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm", "max_roll_arm", "max_picth_arm", "max_yaw_arm", "min_roll_arm", "min_pitch_arm", "min_yaw_arm", "amplitude_roll_arm", "amplitude_pitch_arm", "amplitude_yaw_arm", "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell", "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_roll_dumbbell", "max_picth_dumbbell", "max_yaw_dumbbell", "min_roll_dumbbell", "min_pitch_dumbbell", "min_yaw_dumbbell", "amplitude_roll_dumbbell", "amplitude_pitch_dumbbell", "amplitude_yaw_dumbbell", "var_accel_dumbbell", "avg_roll_dumbbell", "stddev_roll_dumbbell", "var_roll_dumbbell", "avg_pitch_dumbbell", "stddev_pitch_dumbbell", "var_pitch_dumbbell", "avg_yaw_dumbbell", "stddev_yaw_dumbbell", "var_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm", "max_picth_forearm", "max_yaw_forearm", "min_roll_forearm", "min_pitch_forearm", "min_yaw_forearm", "amplitude_roll_forearm", "amplitude_pitch_forearm", "amplitude_yaw_forearm", "var_accel_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm", "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm", "stddev_yaw_forearm", "var_yaw_forearm")
crs$weights   <- NULL

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:10:51 x86_64-w64-mingw32 

# Build a Bosque aleatorio model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(classe ~ .,
  data=crs$dataset[crs$sample, c(crs$input, crs$target)], 
  ntree=500,
  mtry=100,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Bosque aleatorio' model.

crs$rf

# Enumerar la importancia de las variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Tiempo transcurrido: 2.89 mins

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:14:33 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# Leer un conjunto de datos de un archivo para probar el modelo.

crs$testset <- read.csv("C:/Users/Carlos/OneDrive/DS/2017 Coursera PLM/MyTest.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:15:02 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:18:29 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# Leer un conjunto de datos de un archivo para probar el modelo.

crs$testset <- read.csv("C:/Users/Carlos/OneDrive/DS/2017 Coursera PLM/MyTest.cv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 15:18:52 x86_64-w64-mingw32 

# Score a CSV file dataset. 

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Asegúrese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Obtenga calificaciones de probabilidad para el modelo Bosque aleatorio en pml-training.csv.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input),drop=FALSE]))

# Extraer la variables relevantes del conjunto de datos.

sdata <- crs$testset[,]

# Sacar los datos combinados.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Carlos\Desktop\pml-training_score_all.csv", row.names=FALSE)

#============================================================
# Rattle marca de tiempo: 2017-08-27 17:52:22 x86_64-w64-mingw32 

# Recargar los datos del proyecto (crs variables) desde el archivo.

load("C:\Users\Carlos\OneDrive\DS\2017 Coursera PLM\WL.rattle")

#============================================================
# Rattle marca de tiempo: 2017-08-27 17:52:27 x86_64-w64-mingw32 

# Tome en cuenta las selecciones del usuario. 

# Build the train/validate/test datasets.

# nobs=19622 train=13735 validate=2943 test=2944

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$sample   <- crs$train <-sample(nrow(crs$dataset), 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate)

# Se han anotado las siguientes selecciones de variable.

crs$input     <- c("user_name", "raw_timestamp_part_1",
                   "raw_timestamp_part_2",
                   "cvtd_timestamp", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$numeric   <- c("raw_timestamp_part_1",
                   "raw_timestamp_part_2", "num_window",
                   "roll_belt", "pitch_belt", "yaw_belt",
                   "total_accel_belt", "gyros_belt_x",
                   "gyros_belt_y", "gyros_belt_z",
                   "accel_belt_x", "accel_belt_y",
                   "accel_belt_z", "magnet_belt_x",
                   "magnet_belt_y", "magnet_belt_z",
                   "roll_arm", "pitch_arm", "yaw_arm",
                   "total_accel_arm", "gyros_arm_x",
                   "gyros_arm_y", "gyros_arm_z",
                   "accel_arm_x", "accel_arm_y",
                   "accel_arm_z", "magnet_arm_x",
                   "magnet_arm_y", "magnet_arm_z",
                   "roll_dumbbell", "pitch_dumbbell",
                   "yaw_dumbbell", "total_accel_dumbbell",
                   "gyros_dumbbell_x", "gyros_dumbbell_y",
                   "gyros_dumbbell_z", "accel_dumbbell_x",
                   "accel_dumbbell_y", "accel_dumbbell_z",
                   "magnet_dumbbell_x",
                   "magnet_dumbbell_y",
                   "magnet_dumbbell_z", "roll_forearm",
                   "pitch_forearm", "yaw_forearm",
                   "total_accel_forearm",
                   "gyros_forearm_x", "gyros_forearm_y",
                   "gyros_forearm_z", "accel_forearm_x",
                   "accel_forearm_y", "accel_forearm_z",
                   "magnet_forearm_x", "magnet_forearm_y",
                   "magnet_forearm_z")

crs$categoric <- c("user_name", "cvtd_timestamp")

crs$target    <- "classe"
crs$risk      <- NULL
crs$ident     <- "X"
crs$ignore    <- c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", "max_roll_belt", "max_picth_belt", "max_yaw_belt", "min_roll_belt", "min_pitch_belt", "min_yaw_belt", "amplitude_roll_belt", "amplitude_pitch_belt", "amplitude_yaw_belt", "var_total_accel_belt", "avg_roll_belt", "stddev_roll_belt", "var_roll_belt", "avg_pitch_belt", "stddev_pitch_belt", "var_pitch_belt", "avg_yaw_belt", "stddev_yaw_belt", "var_yaw_belt", "var_accel_arm", "avg_roll_arm", "stddev_roll_arm", "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm", "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm", "max_roll_arm", "max_picth_arm", "max_yaw_arm", "min_roll_arm", "min_pitch_arm", "min_yaw_arm", "amplitude_roll_arm", "amplitude_pitch_arm", "amplitude_yaw_arm", "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell", "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_roll_dumbbell", "max_picth_dumbbell", "max_yaw_dumbbell", "min_roll_dumbbell", "min_pitch_dumbbell", "min_yaw_dumbbell", "amplitude_roll_dumbbell", "amplitude_pitch_dumbbell", "amplitude_yaw_dumbbell", "var_accel_dumbbell", "avg_roll_dumbbell", "stddev_roll_dumbbell", "var_roll_dumbbell", "avg_pitch_dumbbell", "stddev_pitch_dumbbell", "var_pitch_dumbbell", "avg_yaw_dumbbell", "stddev_yaw_dumbbell", "var_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm", "max_picth_forearm", "max_yaw_forearm", "min_roll_forearm", "min_pitch_forearm", "min_yaw_forearm", "amplitude_roll_forearm", "amplitude_pitch_forearm", "amplitude_yaw_forearm", "var_accel_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm", "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm", "stddev_yaw_forearm", "var_yaw_forearm")
crs$weights   <- NULL

#============================================================
# Rattle marca de tiempo: 2017-08-27 17:52:30 x86_64-w64-mingw32 

# View the dataset. 

# El paquete 'RGtk2Extras' ofrece la funci�n 'dfedit'.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle marca de tiempo: 2017-08-27 17:52:50 x86_64-w64-mingw32 

# Generar un diagrama de correlaci�n para las variables. 

# El paquete 'corrplot' ofrece la funci�n 'corrplot'.

library(corrplot, quietly=TRUE)

# Las correlaciones solo funcionan para variables num�ricas.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Organice las correlaciones por su fortaleza.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Mostrar las correlaciones reales.

print(crs$cor)

# Mostrar gr�ficamente las correlaciones.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlaci�n pml-training.csv usando Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle marca de tiempo: 2017-08-27 17:53:05 x86_64-w64-mingw32 

# M�quina de vector de apoyo 

# El paquete 'kernlab' ofrece la funci�n 'ksvm'.

library(kernlab, quietly=TRUE)

# Construir un modelo de m�quina de vector de apoyo.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(classe) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="vanilladot",
      prob.model=TRUE)

# Generar una vista textual del modelo SVM.

crs$ksvm

# Tiempo transcurrido: 49.88 segs

#============================================================
# Rattle marca de tiempo: 2017-08-27 17:55:53 x86_64-w64-mingw32 

# Build a Bosque aleatorio model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(classe ~ .,
  data=crs$dataset[crs$sample, c(crs$input, crs$target)], 
  ntree=500,
  mtry=100,
  importance=TRUE,
  na.action=na.omit,
  replace=FALSE)

# Generate textual output of the 'Bosque aleatorio' model.

crs$rf

# Enumerar la importancia de las variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Tiempo transcurrido: 2.96 mins

#============================================================
# Rattle marca de tiempo: 2017-08-27 18:01:28 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generar una matriz de error para el modelo SVM.

# Obtenga la respuesta del modelo SVM.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 18:02:08 x86_64-w64-mingw32 

# Evaluate model performance on a CSV file dataset. 

# Leer un conjunto de datos de un archivo para probar el modelo.

crs$testset <- read.csv("C:/Users/Carlos/OneDrive/DS/2017 Coursera PLM/pml-testing.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Generar una matriz de error para el modelo Bosque aleatorio.

# Obtenga la respuesta del modelo Bosque aleatorio.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$testset[,c(crs$input, crs$target),drop=FALSE])$classe, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#============================================================
# Rattle marca de tiempo: 2017-08-27 18:02:22 x86_64-w64-mingw32 

# Score a CSV file dataset. 

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `user_name'.

levels(crs$testset[["user_name"]]) <- 
  c(levels(crs$testset[["user_name"]]), 
    setdiff(levels(crs$dataset[["user_name"]]), 
               levels(crs$testset[["user_name"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `cvtd_timestamp'.

levels(crs$testset[["cvtd_timestamp"]]) <- 
  c(levels(crs$testset[["cvtd_timestamp"]]), 
    setdiff(levels(crs$dataset[["cvtd_timestamp"]]), 
               levels(crs$testset[["cvtd_timestamp"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `new_window'.

levels(crs$testset[["new_window"]]) <- 
  c(levels(crs$testset[["new_window"]]), 
    setdiff(levels(crs$dataset[["new_window"]]), 
               levels(crs$testset[["new_window"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_belt'.

levels(crs$testset[["kurtosis_roll_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_belt"]]), 
               levels(crs$testset[["kurtosis_roll_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_belt'.

levels(crs$testset[["kurtosis_picth_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_belt"]]), 
               levels(crs$testset[["kurtosis_picth_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_belt'.

levels(crs$testset[["kurtosis_yaw_belt"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_belt"]]), 
               levels(crs$testset[["kurtosis_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt'.

levels(crs$testset[["skewness_roll_belt"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt"]]), 
               levels(crs$testset[["skewness_roll_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_belt.1'.

levels(crs$testset[["skewness_roll_belt.1"]]) <- 
  c(levels(crs$testset[["skewness_roll_belt.1"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_belt.1"]]), 
               levels(crs$testset[["skewness_roll_belt.1"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_belt'.

levels(crs$testset[["skewness_yaw_belt"]]) <- 
  c(levels(crs$testset[["skewness_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_belt"]]), 
               levels(crs$testset[["skewness_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_belt'.

levels(crs$testset[["max_yaw_belt"]]) <- 
  c(levels(crs$testset[["max_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["max_yaw_belt"]]), 
               levels(crs$testset[["max_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_belt'.

levels(crs$testset[["min_yaw_belt"]]) <- 
  c(levels(crs$testset[["min_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["min_yaw_belt"]]), 
               levels(crs$testset[["min_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_belt'.

levels(crs$testset[["amplitude_yaw_belt"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_belt"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_belt"]]), 
               levels(crs$testset[["amplitude_yaw_belt"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_arm'.

levels(crs$testset[["kurtosis_roll_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_arm"]]), 
               levels(crs$testset[["kurtosis_roll_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_arm'.

levels(crs$testset[["kurtosis_picth_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_arm"]]), 
               levels(crs$testset[["kurtosis_picth_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_arm'.

levels(crs$testset[["kurtosis_yaw_arm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_arm"]]), 
               levels(crs$testset[["kurtosis_yaw_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_arm'.

levels(crs$testset[["skewness_roll_arm"]]) <- 
  c(levels(crs$testset[["skewness_roll_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_arm"]]), 
               levels(crs$testset[["skewness_roll_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_arm'.

levels(crs$testset[["skewness_pitch_arm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_arm"]]), 
               levels(crs$testset[["skewness_pitch_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_arm'.

levels(crs$testset[["skewness_yaw_arm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_arm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_arm"]]), 
               levels(crs$testset[["skewness_yaw_arm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_dumbbell'.

levels(crs$testset[["kurtosis_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_dumbbell"]]), 
               levels(crs$testset[["kurtosis_roll_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_dumbbell'.

levels(crs$testset[["kurtosis_picth_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_dumbbell"]]), 
               levels(crs$testset[["kurtosis_picth_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_dumbbell'.

levels(crs$testset[["kurtosis_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_dumbbell"]]), 
               levels(crs$testset[["kurtosis_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_dumbbell'.

levels(crs$testset[["skewness_roll_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_roll_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_dumbbell"]]), 
               levels(crs$testset[["skewness_roll_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_dumbbell'.

levels(crs$testset[["skewness_pitch_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_pitch_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_dumbbell"]]), 
               levels(crs$testset[["skewness_pitch_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_dumbbell'.

levels(crs$testset[["skewness_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["skewness_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_dumbbell"]]), 
               levels(crs$testset[["skewness_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_dumbbell'.

levels(crs$testset[["max_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["max_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["max_yaw_dumbbell"]]), 
               levels(crs$testset[["max_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_dumbbell'.

levels(crs$testset[["min_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["min_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["min_yaw_dumbbell"]]), 
               levels(crs$testset[["min_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_dumbbell'.

levels(crs$testset[["amplitude_yaw_dumbbell"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_dumbbell"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_dumbbell"]]), 
               levels(crs$testset[["amplitude_yaw_dumbbell"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_roll_forearm'.

levels(crs$testset[["kurtosis_roll_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_roll_forearm"]]), 
               levels(crs$testset[["kurtosis_roll_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_picth_forearm'.

levels(crs$testset[["kurtosis_picth_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_picth_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_picth_forearm"]]), 
               levels(crs$testset[["kurtosis_picth_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `kurtosis_yaw_forearm'.

levels(crs$testset[["kurtosis_yaw_forearm"]]) <- 
  c(levels(crs$testset[["kurtosis_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["kurtosis_yaw_forearm"]]), 
               levels(crs$testset[["kurtosis_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_roll_forearm'.

levels(crs$testset[["skewness_roll_forearm"]]) <- 
  c(levels(crs$testset[["skewness_roll_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_roll_forearm"]]), 
               levels(crs$testset[["skewness_roll_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_pitch_forearm'.

levels(crs$testset[["skewness_pitch_forearm"]]) <- 
  c(levels(crs$testset[["skewness_pitch_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_pitch_forearm"]]), 
               levels(crs$testset[["skewness_pitch_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `skewness_yaw_forearm'.

levels(crs$testset[["skewness_yaw_forearm"]]) <- 
  c(levels(crs$testset[["skewness_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["skewness_yaw_forearm"]]), 
               levels(crs$testset[["skewness_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `max_yaw_forearm'.

levels(crs$testset[["max_yaw_forearm"]]) <- 
  c(levels(crs$testset[["max_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["max_yaw_forearm"]]), 
               levels(crs$testset[["max_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `min_yaw_forearm'.

levels(crs$testset[["min_yaw_forearm"]]) <- 
  c(levels(crs$testset[["min_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["min_yaw_forearm"]]), 
               levels(crs$testset[["min_yaw_forearm"]])))

# Aseg�rese de que los niveles sean los mismos que los datos de entrenamiento para la variable `amplitude_yaw_forearm'.

levels(crs$testset[["amplitude_yaw_forearm"]]) <- 
  c(levels(crs$testset[["amplitude_yaw_forearm"]]), 
    setdiff(levels(crs$dataset[["amplitude_yaw_forearm"]]), 
               levels(crs$testset[["amplitude_yaw_forearm"]])))

# Obtenga calificaciones de probabilidad para el modelo Bosque aleatorio en pml-training.csv.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input),drop=FALSE]))

# Extraer la variables relevantes del conjunto de datos.

sdata <- subset(crs$testset[,], select=c("X", "classe"))

# Sacar los datos combinados.

write.csv(cbind(sdata, crs$pr), file="C:\Users\Carlos\Desktop\pml-training_score_idents.csv", row.names=FALSE)