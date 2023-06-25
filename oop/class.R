library(R6)
library(terra)
library(xml2)
library(kableExtra)
load(file = "./waterquality/data/wq_algorithms.rda")

Waterquality <- R6Class("Waterquality",
  public = list(
    path = NULL,
    type = NULL,
    bands = NULL,

    initialize = function(path, type = NULL, bands = NULL) {
      self$path <- path
      self$type <- type
      self$bands <- bands

      if (is.null(self$type)) {
        self$type <- private$get_type()
      }
    },

    calc_index = function(index) {
      index_met <- subset(wq_algorithms, name == index & satellite == self$type)
      index_function <- private$get_index_function(index)
      func <- eval(parse(text = index_function))
      bands <- private$get_bands(index)
      result <- do.call(func, as.list(bands))
      result
    }
  ),

  private = list(
    get_type = function() {
      file <- list.files(self$path,
                        pattern = "_MTL.txt$|MTD_",
                        full.names = TRUE)
      if (grepl("_MTL.txt", file)) {
        lines <- readLines(file)
        matching_line <- grep("SPACECRAFT_ID\\s*=\\s*", lines, value = TRUE)
        spacecraft_id <- sub('.*SPACECRAFT_ID\\s*=\\s*"([^"]+)".*', "\\1",
                              matching_line)
        type <- gsub("_", "", tolower(spacecraft_id))
      } else if (grepl("MTD_", file)) {
        xml <- read_xml(file)
        spacecraft_name <- xml_text(xml_find_all(xml, ".//SPACECRAFT_NAME"))
        type <- gsub("-|a|b", "", tolower(spacecraft_name))
      }
      type
    },

    get_bands = function(index) {
      result <- list()

      if (is.null(self$bands)) {
        values <- subset(wq_algorithms,
        name == index & satellite == self$type, select = "bands")
        self$bands <- unlist(values$bands)
      }
      for (band in self$bands) {
        if (self$type == "landsat8") {
          pattern <- paste0("B", band, ".TIF$")
          band_path <- list.files(self$path, pattern, full.names = TRUE)
        } else if (self$type == "sentinel2") {
          if (nchar(band) == 1) {
            pattern <- paste0("B0", band, ".jp2")
          } else {
            pattern <- paste0("B", band, ".jp2")
          }
          path_granule <- list.dirs(file.path(self$path, "GRANULE"),
                                    full.names = TRUE,
                                    recursive = FALSE)
          path_subdir <- path_granule[1]
          band_path <- list.files(file.path(path_subdir, "IMG_DATA"),
                                  pattern,
                                  full.names = TRUE)
        }
        raster <- rast(band_path)
        result <- append(result, raster)
      }
      result
    },

    get_index_function = function(index) {
      index_function <- subset(wq_algorithms,
                              name == index & satellite == self$type,
                              select = "funs")
      index_function <- unlist(index_function)
    }
  ),

  active = list(
    indexes = function() {
      index_met <- subset(wq_algorithms,
                          satellite == self$type,
                          select = c("name", "type", "bands")) %>%
                          kbl() %>%
                          kable_styling()
      index_met
    }
  )
)

waterquality <- function(path, type = NULL, bands = NULL) {
  Waterquality$new(path, type, bands)
}

landsat8 <- waterquality(path = "./LC08_L1TP_125059_20210617_20210622_02_T1")
landsat8$indexes
TurbMoore80Red <- landsat8$calc_index("TurbMoore80Red")
plot(TurbMoore80Red)

sentinel2 <- waterquality(path = "./S2A_MSIL1C_20160903T031542_N0204_R118_T48NUG_20160903T033244.SAFE", bands = c(3, 4, 2))
sentinel2$indexes
TurbFrohn09GreenPlusRedBothOverBlue <- sentinel2$calc_index("TurbFrohn09GreenPlusRedBothOverBlue")
plot(TurbFrohn09GreenPlusRedBothOverBlue)
