#this scripts adjusts the VEET import function in case one uses an older version of LightLogR (≤0.9.2)

VEET_expr <- list(
VEET = rlang::expr({
  #separate the dots list in the column_names and the rest
  dots <- rlang::list2(...)
  modality <- dots$modality
  dots$modality <- NULL
  stopifnot(modality %in% c("ALS", "IMU", "INF", "PHO", "TOF"))
  veet_names <- list(
    ALS = c(time_stamp = TRUE, modality = FALSE, integration_time = TRUE, 
            uvGain = TRUE, visGain = TRUE, irGain = TRUE, uvValue = TRUE, 
            visValue = TRUE, irValue = TRUE, Flicker = TRUE, Lux = TRUE),
    IMU = c(time_stamp = TRUE, modality = FALSE, ax = TRUE, ay = TRUE, 
            az = TRUE, gx = TRUE, gy = TRUE, gz = TRUE, temp = TRUE),
    INF = c(time_stamp = TRUE, modality = FALSE, product_name = FALSE, 
            serial_number = FALSE, fw_version = FALSE, Researcher_ID = FALSE, 
            Participant_ID = FALSE, Time_Zone_offset = TRUE, IMU_interval = TRUE, 
            PHO_interval = TRUE, TOF_interval = TRUE, ALS_interval = TRUE, 
            Temple_Config = FALSE, TOF_Iterations = TRUE, 
            IMU_Cal_Table = TRUE, PHO_Cal_Table = TRUE, 
            ToF_Cal_Table = TRUE, ALS_Cal_Table = TRUE, 
            Unit_Timestamp = FALSE, Unit_Batt_Voltage = FALSE, 
            Unit_Sensor_Interval = FALSE, Unit_IMU_Accel = FALSE, 
            Unit_IMU_Gyro = FALSE, Unit_Pho_Cts = FALSE, Unit_Pho_Gain = FALSE, 
            Unit_ToF = FALSE, Unit_ALS_Cts = FALSE, Unit_ALS_Gain = FALSE, 
            Unit_ALS_Flicker = FALSE),
    PHO = c(time_stamp = TRUE, modality = FALSE, integration_time = TRUE, 
            Gain = TRUE, s415 = TRUE, s445 = TRUE, s480 = TRUE, s515 = TRUE, 
            s555 = TRUE, s590 = TRUE, s630 = TRUE, s680 = TRUE, s910 = TRUE, 
            Dark = TRUE, Clear = TRUE),
    TOF = stats::setNames(
      c(TRUE, FALSE, rep(TRUE, 4 * 64)), 
      c("time_stamp", "modality", 
        paste0("conf1_", 0:63), 
        paste0("conf2_", 0:63), 
        paste0("dist1_", 0:63), 
        paste0("dist2_", 0:63))
    )
  )
  data <- 
    purrr::map(filename, \(filename) {
      pattern <- paste0("^(?:[^,]*,){1}\\b", modality, "\\b")
      data <- 
        readr::read_lines(file = filename, locale = locale, n_max = n_max)
      data <- data[data %>% stringr::str_detect(pattern)]
      data <- stringr::str_split(data, ",") %>% 
        purrr::list_transpose() %>% list2DF()
      names(data) <- names(veet_names[[modality]])
      data <- data %>% 
        dplyr::mutate(file.name = filename, .before = 1)
      data
    }) %>% purrr::list_rbind()
  data <- data %>% 
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(
          veet_names[[modality]][veet_names[[modality]]] %>% names()), 
        as.numeric),
      Datetime = lubridate::with_tz(
        lubridate::as_datetime(time_stamp, tz = "UTC"), tz), .before = 1
    )
}
)
)

import <- import_adjustment(VEET_expr)

