
#'Simple function that takes a file name as an input and reads a .csv file
#'
#'The file has to be present in the source directory. If a file is not present, the function stops execution
#'
#'@param filename A character string of a file name
#'
#'@return The function returns a dataframe. No modifications to the source file
#'
#'@importFrom dplyr tbl_df
#'@importFrom readr read_csv
#'
#'@inheritParams make_filename
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Simple function that takes a year as a number and merges it within the template accident_YEAR.csv.bz2
#'
#'@param year number or string that can be coerced to a number
#'
#'@return The function returns a string that will be used as file input
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'Read files from the source directory based on a vector of years
#'
#'The function takes a vector of years and reads files with each year in the name into a list
#'@param years a vector of years to be included in to a file name
#'
#'@inheritParams make_filename
#'@inheritParams fars_read
#'
#'@importFrom dplyr mutate select
#'@importFrom magrittr %>%
#'
#'@return This function returns a list with each element containing data for each year in the input
#'
#'@examples
#'\dontrun{
#'df<-fars_read_years(c(2014,2015,2016))
#'}
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#'Produce summary counts from the specified years
#'
#'This function takes a vector of years, load the files based on the template from make_file name and counts records
#'by year, by month
#'@inheritParams fars_read_years
#'
#'@return a dataframe that counts number of records by year, by month
#'
#'@importFrom dplyr bind_rows summarize group_by
#'@importFrom tidyr spread
#'
#'@examples
#'\dontrun{
#'df2<-fars_summarize_years(c(2014,2015,2016))
#'}
#'
#'@export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#'Plot accidents by sate by year
#'
#'This function takes state number and a year and creates a plot of accidents overlayed over a polygon of a state
#'@param state.num a numeric value normally between 1 and 52. The value is validated against unique values in the dataframe
#'
#'@inheritParams make_filename
#'@inheritParams fars_read
#'
#'@return This function produces a plot and returns NULL
#'
#'@importFrom maps map
#'@importFrom dplyr filter
#'
#'@examples
#'\dontrun{
#'fars_map_state(10,2014)
#'}
#'
#'@export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
