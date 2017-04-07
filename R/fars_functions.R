#' fars_read
#'
#' Function reads data from an external csv file
#'
#' @param filename is the name (string) of the file that will be read
#'
#' @return Function returns a table of data from the specified file
#'
#' @note This function will fail if a file with the specified name does not exist
#'
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' make_filename
#'
#' Function creates the name of the file to be read based on the specified year
#'
#' @param year is the year of the data to be examined ... year can be integer or string (because input will be converted to integer)
#'
#' @return Function returns a file name that includes the specified year
#'
#' @examples
#'    make_filename(2013)
#'    make_filename(2015)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' Function reads the data for the years specified
#'   importFrom dplyr %>%
#' @import dplyr
#' @param years is the set of years for which the user is requesting data tables or maps
#'
#' @return Function returns a table for each requested year. Each table contains columns for MONTH and year
#'
#' @note This function will fail if an invalid year is specified, if the "years" imply a file that does not exist
#'
#' @export
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

#' fars_summarize_years
#'
#' This function calls other functions so that it eventually returns a table to the user of data corresponding to the years specified by user
#'
#' @import dplyr
#'
#' @param years is the set of years (integers) for which the user is requesting tables of data
#'
#' @return Function returns a table that contains data for Month and year plus summary lines in the table tabulate the number of entries for each MONTH and year group
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}


#' fars_map_state
#'
#' This function displays maps of data for the year specified by user for a state specified by the user
#'
#' @param state.num is the ID number (integer) for the state for which the user is requesting a map
#' @param year is the year (integer) of the data the user wants to see mapped for the requested state
#'
#' @import maps
#'
#' @return Function returns a data map of the data for the requested state
#'
#' @note This function will fail if an invalid state ID number (state.num), is specified
#'
#' @note -- state numbers from 1 t 50 plus, but don't include 2 or 3
#'
#' @export
#'
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
