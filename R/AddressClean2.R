#' AddressR package
#'
#' This function communicates with the GNAF geocoding API to return an official address.
#' @param nonblanks a dataframe including addresses with a number first field
#' @keywords GNAF, Geocode, fuzzymatching, Address
#' @export
#' @examples
#' AddressClean2()


AddressClean2<- function(nonblanks, ID) {

  GNAF_address <- data.table::fread("C:/Users/farrelp1/Documents/AddressR/data/May17_GNAF_address.csv", stringsAsFactors = F)

  options <- lapply(nonblanks$KEY, function(x) which(x == GNAF_address$KEY))

  names(options) <- nonblanks$ID

  totalscore <- lapply(seq_along(options), function(i) parallelAddressTotalScore3(nonblanks[i,], GNAF_address[options[[i]],]))

  options2 <- lapply(seq_along(options), function(i) suppressWarnings(options[[i]][which(totalscore[[i]] == min(totalscore[[i]]))]))

  names(options2) <- names(options)

  result <- list()

  options3 <- options2[unname(unlist(lapply(options2, function(x) length(x) > 0)))]

  lapply(names(options2), function(i) result[[i]] <<- GNAF_address[options2[[i]],])

  finalresult <- plyr::ldply(result, function(x) x %>% filter(CONFIDENCE == max(CONFIDENCE))) #%>%
                 #mutate(ID = as.numeric(.id))

  #dup <- finalresult[duplicated(finalresult$.id),]

  #CWW_TradeWaste_ABR <- select(finalresult, AG_CONSUMERNUMBER, ADDRESS_DETAIL_PID) %>% distinct()

  return(finalresult)

}
