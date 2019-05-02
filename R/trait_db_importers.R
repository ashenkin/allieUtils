#TODO allow passing of filenames as arguments

#' @export
import_glopnet <- function(file = NA) {
    wb = XLConnect::loadWorkbook(glopnet_file)
    glopnet = XLConnect::readWorksheet(wb, sheet = 1, startRow = 11, check.names = T)
    return(glopnet)
}

#' @export
import_try <- function(traits = NA, with_categ = T, with_site = T, use_cache = F, try_dir = NA, traits_file = NA, sites_file = NA, categ_file = NA) {
    # for now, traits should be a vector of integers pointing to TraitID in the TRY dataset
    #try_traits = data.table::fread(try_traits_file, sep = "\t", fill = TRUE)
    try_traits = read.table(try_traits_file, sep = "\t", stringsAsFactors = FALSE, header = TRUE, quote = "", fill = T)
    if (with_site) {
        try_sites = data.table::fread(try_site_file)
        try_traits = merge(try_traits, try_sites[,.(ObservationID, LAT_site, LON_site)], by = "ObservationID", all.x=TRUE)
    }
    if (with_categ) {
        try_categ = data.table::fread(try_categ_file)
        try_traits = merge(try_traits, try_categ[, c(1,4:ncol(try_categ)), with=F], by = "AccSpeciesID", all.x=TRUE)
    }
    try_traits = data.frame(try_traits)
    if (is.na(traits)) {
        return( data.frame(try_traits) )
    } else {
        return( data.frame(try_traits[TraitID %in% traits]) )
    }
}

#' @export
import_baad <- function(file = NA) {
    baad = read.csv(baad_file, stringsAsFactors = F)
    return(baad)
}

#' @export
import_global_wd_db <- function(file = NA) {
    wb = XLConnect::loadWorkbook(gwdd_file)
    gwdd = XLConnect::readWorksheet(wb, sheet = "Data", header=T, check.names = T)
    names(gwdd)[4] = "wd"
    return(gwdd)
}
