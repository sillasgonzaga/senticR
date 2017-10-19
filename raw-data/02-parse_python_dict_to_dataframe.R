library(tidyverse)
library(stringr)
library(qdap)
library(magrittr)
library(devtools)

dict_files <- dir("/home/sillas/R/Projetos/senticnet-4.0", pattern = "*.py", recursive = TRUE,
                  full.names = TRUE)
# define names of files
names(dict_files) <- tools::file_path_sans_ext(basename(dict_files))
# replace senticnet4with english
names(dict_files)[names(dict_files) == "senticnet4"] <- "senticnet_en"


convert_dict_to_dataframe <- function(py_file, py_file_name){

  df <- read.table(py_file, header = FALSE, sep = ",", skip = 1, stringsAsFactors = FALSE)
  # separate first column using = as a separator
  df <- df %>% separate(V1, c("V01", "V02"), sep = "=")
  df$V01 <- unlist(bracketXtract(df$V01, bracket = "square"))
  # replace _ with space (as it makes more sense)
  df$V01 <- str_replace_all(df$V01, "_", " ")
  # drop V12 (it's the same as V02)
  df$V02 <- as.numeric(str_replace_all(df$V02, "\\[", ""))
  df$V12 <- str_replace_all(df$V12, "\\]", "")
  # for english lexicon, remove V07 (unnecessary polarity column (positive or negative))
  if (py_file_name == "senticnet_en") {
    df$V7 <- NULL
    df$V13 <- str_replace_all(df$V13, "\\]", "")
  }

  # check if dataframe has 13 columns
  if (ncol(df) != 13){
    stop("The dataframe does not have 13 columns")
  }
  # check if there is any duplicate
  stopifnot(length(unique(df$V01)) == nrow(df))

  # define colnames
  colnames(df) <- c(
    "term",
    "sentic.pleasantness", "sentic.attention", "sentic.sensivity", "sentic.aptitude",
    "moodtag1", "moodtag2", "polarity.intensity",
    "semantic_1", "semantic_2", "semantic_3", "semantic_4", "semantic_5"
  )
  # trim character variables
  df %<>% mutate_if(is.character, str_trim)
  
  
  ## save data to be used later in the package as data() objects
  out_filename <- paste0(py_file_name, ".rda")
  out_folder <- "data/"
  if (!dir.exists(out_folder)) dir.create(out_folder)
  out_filename <- paste0(out_folder, out_filename)
  # save dataframe with correct name (df becomes senticneg_LANG)
  data_frame_name <- py_file_name
  assign(data_frame_name, df)
  
  save(list = data_frame_name, file = out_filename)
  #saveRDS(df, out_filename)
}


error <- c("cz", "fi", "ht", "hu", "id", "jp", "nl", "ru", "ua", "ur", "tr")
error <- error %>% str_c("_", .) %>% str_c(collapse = "|")
dict_files <- dict_files[!str_detect(dict_files, error)]

system.time({
  list(dict_files, names(dict_files)) %>% pmap(convert_dict_to_dataframe)
})


# en <- "/home/sillas/R/Projetos/senticnet-4.0/senticnet4.py"
# name_en <- "senticnet_en"
# 
# convert_dict_to_dataframe(en, name_en)
# Use devtools to store data as pkg-usable

