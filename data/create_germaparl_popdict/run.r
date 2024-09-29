library(polmineR)

n_cores <- parallel::detectCores()
options('polmineR.cores' = n_cores - 1)
data.table::setDTthreads(n_cores - 1)

here::here()

ptm <- proc.time()

apply_popdict <- function(protocol) {
  # split up the protocol by speaker and apply popdict
  by_speaker <- protocol |>
    partition_bundle(s_attribute = "speaker_name") |>
    as.VCorpus() |>
    quanteda::corpus() |>
    popdictR::run_popdict() |>
    quanteda::convert(to="data.frame") |>
    as.data.table()
  
  by_speaker[, c("doc_id", "speaker") := NULL]
  
  # extract the protocol information
  names = stringr::str_replace_all(protocol@s_attributes$protocol, '="[^"]*"', '') |>
    stringr::str_split(" ") |>
    unlist()
  values_str = protocol@s_attributes$protocol |>
    stringr::str_replace_all('[a-z]*=', '') |> 
    stringr::str_replace_all('"', '')
  protocol_metadata_df <- read.table(text=values_str, col.names = names)
  
  # Merge Protocol attributes with result df
  result <- cbind(
    by_speaker,
    protocol_metadata_df[rep(1, nrow(by_speaker)), ]
  )
  
  return(result)
}


PROGRESS_FILE <- 'last_index.txt'
OUT_FILE <- 'germaparl_popdict.csv'

if (file.exists(PROGRESS_FILE)) {
  last_index <- as.integer(readLines(PROGRESS_FILE))
} else {
  last_index <- 0
}

protocols_bundle = corpus("GERMAPARL2") |>
  partition_bundle(s_attribute = "protocol")

protocols_list <- protocols_bundle@objects

result_dts <- list()
for (i in (last_index+1):length(protocols_list)) {
  protocol <- protocols_list[[i]]
  
  # compute
  protocol_dt_with_popdict_by_speaker <- apply_popdict(protocol)
  
  # save batch
  data.table::fwrite(protocol_dt_with_popdict_by_speaker, file=OUT_FILE, append=TRUE)
  
  # update progress
  writeLines(as.character(i), PROGRESS_FILE)
  
  print(proc.time() - ptm)
  print(i)
  print("of")
  print(length(protocols_list))
}

