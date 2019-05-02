# File to hold functions

####################
# Plot settings ####
####################
# crown plotting colors
library(RColorBrewer)
library(ggplot2)
library(viridis)
# crown_point_colors <- brewer.pal(3,"Set1")
crown_point_colors <- c("blue", "green", "black")
names(crown_point_colors) = c("top","bot","perim")
crown_point_colScale <- scale_colour_manual(name = "crown point type",values = crown_point_colors)
crown_point_shapeScale <- scale_shape_manual(name = "crown point type", values=c("top"=17, "bot"=17, "perim"=16))
crown_point_sizeScale <- scale_size_manual(name = "crown point type", values=c("top"=3, "bot"=3, "perim"=2))

#forest_class_colors <- c("Light Goldenrod 1", "olive drab 2", "olive drab 4", "blue", "black")
forest_class_colors <- rev(viridis(5))#c(viridis(4), viridis(1, begin = 1, end = 1)))
names(forest_class_colors) = c("Savanna", "Transitional", "TF", "TMCF", "All")
forest_class_shapes = c(0,1,15,16,8)
names(forest_class_shapes) = c("Savanna", "Transitional", "TF", "TMCF", "All")
forest_class_shapeScale <- scale_shape_manual(name = "Ecosystem", values = forest_class_shapes)
forest_class_colScale <- scale_color_manual(name = "Ecosystem", values = forest_class_colors)
forest_class_fillScale <- scale_fill_manual(name = "Ecosystem", values = forest_class_colors)

region_colors <- c("#FF9C63FF", "#C729D6FF", "#1400FFFF", "#000033FF") # yellow-pink-blue scale.  see sp::bpy.colors().
names(region_colors) <- c("Brazil", "Ghana", "Peru", "All")
region_colScale <-  scale_color_manual(name = "Region", values = region_colors)

peru_ele <- c(215, 223, 595, 859, 1494, 1713, 2719, 2868, 3045, 3537)
peru_ele_colScale <- rev(viridis(3537-214)[peru_ele - min(peru_ele) + 1])
names(peru_ele_colScale) <- peru_ele

background_rect = data.frame(xstart = c(1,10,14), xend = c(10, 14, 20), region = c("Peru", "Brazil", "Ghana"), stringsAsFactors = F)
background_rect = data.frame(xstart = c("ACJ01", "CRP01", "KOG04"), xend = c("CRP01", "KOG04", "ANK01"), region = c("Peru", "Brazil", "Ghana"), stringsAsFactors = F)
forest_class_plus_bkgnd_colors = c(forest_class_colors, region_colors[1:3])
forest_class_plus_bkgnd_fillScale = scale_fill_manual(name = "Ecotype_region", values = forest_class_plus_bkgnd_colors)

varcomp_colors = c("#AAAAAAFF", viridis(100)[rev(c(1, 15, 30, 60, 100))])
varcomp_fillScale <- scale_fill_manual(values = varcomp_colors)




#     region_colors <- c("red", "darkgreen", "blue")
#     names(region_colors) <- c("Brazil", "Ghana", "Peru")
#     region_colScale <- scale_color_manual(name = "Region", values = region_colors)

#     regions <- c("Brazil", "Ghana", "Peru")
#     region_colScale = scale_color_brewer(regions, type = "qual", palette = "Set1")

# Functions ####

library(gridExtra)
# from http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

# from http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots/28594060#28594060
grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    plot_list = lapply(plots, function(x) x + theme(legend.position="none", axis.title.x = element_blank()))
    plot_list["ncol"] = 1
    return(
        grid.arrange(
            do.call(arrangeGrob, plot_list),
            legend,
            ncol = 1,
            heights = unit.c(unit(1, "npc") - lheight, lheight))
    )
}

factor.to.numeric <- function(x) {
    # necessary for converting factor to numeric.  Converting factors to numeric
    # with as.numeric gets their ranks, not levels. This is kludgy, since I assume
    # all numeric columns have been automatically converted to factors by
    # as.data.frame.  It would be better to address this in the as.data.frame
    # call, but there is no colClasses argument available to force column types.
    as.numeric(levels(x))[as.integer(x)]
}

factor.to.integer <- function(x) {
    as.integer(levels(x))[as.integer(x)]
}

set_df_coltypes <- function (df, col_types) {
    # Function to set column types in dataframes.  col_types is a character vector or a dataframe whose column types you want to duplicate.
    # col_types is recycled if ncol(df) > length(col_types)

    if (class(col_types) == "data.frame") {
        col_types = unlist(lapply(col_types, class))
    }

    for (i in 1:length(df)) {
        if (any(class(df) == "tbl"))
            invec = pull(df, i)
        else
            invec = df[,i]
        #df[,i] = coerce_fun[[ col_types[(i-1) %% length(col_types) + 1] ]]( df[,i] ) #apply coerce function
        df[,i] = coerce(invec, col_types[(i-1) %% length(col_types) + 1])
    }
    #lapply(coerce_fun, `[`, col_types) #list of functions to apply
    return(df)
}

coerce <- function( invar, totype ) {
    # coerce a varible to the appropriate type.  totype = character|factor|numeric|integer|POSIXct|logical
    coerce_fun = list (
        "character"   = `as.character`,
        "factor"      = `as.factor`,
        "numeric"     = `as.numeric`,
        "integer"     = `as.integer`,
        "POSIXct"     = `as.POSIXct`,
        "logical"     = `as.logical`,
        "date"        = `as.Date` )

    coerce_fun[[ totype ]]( invar )
}

as.level <- function( fac, level_class = "integer" ) {
    # get levels of a factor, instead of the default behavior which returns ordinal position when as.* is used
    #  pass in level_class for the class of the levels to output, default behavior is to treat as integer

    if (level_class == "integer") {
        levels_out = as.integer( levels(fac) )[as.integer(fac)]

    } else if (level_class == "character") {
        levels_out = as.character( levels(fac) )[as.integer(fac)]

    } else if(level_class == "numeric") {
        levels_out = as.numeric( levels(fac) )[as.integer(fac)]
    }

    return( levels_out )
}

replace_in_dataframe <- function(df, search, replace) {
    #replace a value in a dataframe.  factors are an issue at the moment, as NA might be an invalid level.
    for (i in names(df)) {
        df[[i]][ df[[i]] %in% search ] = replace  # can't use == since NA's become a problem
    }

    return( df )
}

replace_NA_vec <- function(vec, replace) {
    vec[which(is.na(vec))] = replace
    return(vec)
}

imp_data <- function(xlc_wb, sheet, col_types) {
    # Due to Java conversion weirdness in XLConnect, convert data types after import (ie don't use colTypes)
    df_out = readWorksheet(xlc_wb, sheet = sheet, colTypes = "character")
    setMissingValue(xlc_wb, c("missing"))  # This doesn't yet work for values that Java groks as numeric,
    # so use replace_in_dataframe until it's fixed
    df_out = replace_in_dataframe(df_out, c("999", "999.0"), NA)
    df_out = xlc_dot_zero_hack(df_out)
    # convert char columns to factors, since XLConnect doesn't support factor datatype
    return( reset_col_types(df_out, col_types = col_types) )
}

xlc_dot_zero_hack <- function(df) {
    # looks for all numerics that had a ".0" added to them by the buggy XLConnect, and removes the ".0"
    converted_list = lapply( df, FUN = function(x) sub("^(\\d*)\\.0$", "\\1", x, perl=TRUE))
    return(
        as.data.frame( converted_list, stringsAsFactors = FALSE )
    )
}

format_cols <- function(x, format_list = list(), signif_codes=FALSE) {
    # function to facilitate column-by-column reformatting of data
    # Mostly used to format for printing (i think)
    # usage: length(format_list) may equal ncol(x).  If shorter, it will be recycled.
    #        format list should be a list of lists of key=value pairs corresponding to format settings for each column
    if (is.matrix(x) | is.data.frame(x)) {
        xout = x
        for (i in 1:ncol(x)){
            xout[,i] = do.call("format", c(list(x=x[,i]), format_list[[(i-1) %% length(format_list) + 1]]))
        }
        if (signif_codes) {
            # run significance code on last column in matrix/dataframe
            xout <- cbind( xout,
                           symnum(x[,ncol(x)], corr = FALSE, na = FALSE,
                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                  symbols = c("***", "**", "*", ".", " ")) )
            colnames(xout)[ncol(xout)] = "Signif"
        }
        return(xout[,])
    } else if (is.list(x)) {
        xout=list()
        for (i in 1:length(x)){
            xout[[i]] = c(do.call("format", c(list(x=x[[i]]), format_list[[(i-1) %% length(format_list) + 1]])))
        }
        names(xout) = names(x)
        return(xout)
    }
}

na.omit.somecols <- function(data, noNAsInTheseCols, allOutputCols = names(data)) {
    completeVec <- complete.cases(data[, noNAsInTheseCols])
    return(data[completeVec, allOutputCols])
}


triangles_list = list()

compute_poly_vol <- function(perimeters, top, bottom) {
    # perimeters is a 3-column matrix of points taken in order around the crown
    # top and bottom are vectors
    # Solution from http://www.gamedev.net/topic/662808-calculate-volume-of-non-convex-polyhedron/

    triangles = list()
    for (i in 1:nrow(perimeters)) {
        triangles[[i]] = matrix(c(perimeters[i,], perimeters[ifelse((i+1) > nrow(perimeters),1,i+1),], top), ncol=3, byrow=T)
        triangles[[i + nrow(perimeters)]] = matrix(c(perimeters[ifelse((i+1) > nrow(perimeters),1,i+1),], perimeters[i,], bottom), ncol=3, byrow=T)
    }
    triangles_list[[.GlobalEnv$i]] <<- triangles
    return(sum(sapply(triangles,det))/6)

    #subset(perimeters, treecode == "ESP01-T52")[,c("x","y","z")]

}

xlate_col_names <- function(col_names_xlate, col_names, old_dataf) {
    # function that will take columns from old_dataf and morph them into a new dataframe given new column names and a translation list
    # The translation list should have old column names as keys, and new column names as values
    first_run = T
    for (this_col_name in col_names) {
        crown_col_name = col_names_xlate[[this_col_name]]
        if (is.na(crown_col_name)) {
            this_col = rep(NA,nrow(old_dataf))
        } else {
            this_col = old_dataf[,crown_col_name]
        }
        if (first_run) {
            new_dataf = as.data.frame(this_col)
            first_run = F
        } else {
            new_dataf = cbind(new_dataf, this_col)
        }
    }
    colnames(new_dataf) = col_names
    return(new_dataf)
}

clean_plot_codes <- function(plot_codes) {
    plot_names = sub("^\\s*([A-Za-z]{3}).*$","\\1",plot_codes)
    plot_nums = as.integer(sub("^.*?(\\d{1,2})\\s*$","\\1",plot_codes))
    plot_nums = sprintf("%02d", plot_nums)
    plot_codes = paste(plot_names, plot_nums, sep = "-")
    return(plot_codes)
}

join_site <- function(df, site_col = "site", remove_dash = T, site_factor = T) {
    library(dplyr)
    site_data = read_site_data()
    if (remove_dash) {
        site_data$site = sub("-","",site_data$plot_code)
    } else {
        site_data$site = site_data$plot_code
    }
    site_data$site = as.character(site_data$site)
    df = left_join(df, site_data, by = setNames("site", site_col))
    #levels(df$region) = c(levels(df$region), "All")
    #df$region[is.na(df$region)] = "All"
    levels(df$eco_class) = c(levels(df$eco_class), "All")
    #df$eco_class[is.na(df$eco_class)] = "All"
    if (site_factor) {  # this was clobbering data when site column contained model coef names that weren't all site names
        df[,site_col] = factor(df %>% .[[site_col]], levels = site_data$site[order(site_data$overall_order)])
    }
    return(df)
}

query_higher_taxa_classes <- function(species_list, known = "genus", order = c("dataframe", "unique_sp")) {
    # Pass in a character vector of species, genera, families, or whatever (the search is flexible)
    # Returns a dataframe with the columns: query, db, family, order, subdivision
    # The dataframe returned is guaranteed to be in the same order as the species list passed in if order is "dataframe"
    order = match.arg(order)

    library(taxize)
    library(plyr)
    species_list = sub("^([^ ]*).*$","\\1",species_list) # just take the top level name before the space
    # remove short names that clog the taxon query - replace later
    which_short = which(nchar(species_list) < 3)
    short_names = species_list[which_short]
    species_list[which_short] = "TEMPTAXON"
    # agglomerate to be more efficient
    species_list_uniq = unique(species_list)
    if (known == "genus") {
        highertaxa <- tax_name(species_list_uniq,get=c("family", "order", "subdivision"), db = "both")
    } else if (known == "family") {
        highertaxa <- tax_name(species_list_uniq,get=c("order", "subdivision"), db = "both")
    }

    highertaxa_ncbi = subset(highertaxa, db == "ncbi")
    highertaxa_itis = subset(highertaxa, db == "itis")

    merdat <- merge(highertaxa_ncbi, highertaxa_ncbi, by="query")  # seems self-documenting
    merdat$family.y[ is.na(merdat$family.y) ] <- merdat$family.x[ is.na(merdat$family.y) ]
    merdat$order.y[ is.na(merdat$order.y) ] <- merdat$order.x[ is.na(merdat$order.y) ]
    merdat$subdivision.y[ is.na(merdat$subdivision.y) ] <- merdat$subdivision.x[ is.na(merdat$subdivision.y) ]
    merdat = merdat[,c(1,grep("\\.x",names(merdat)))]
    names(merdat) = sub("\\.x","",names(merdat))
    highertaxa = merdat

    species_list[which_short] = short_names # replace short names removed earlier
    ret_df = join(data.frame(query = species_list), highertaxa, by = "query", type = "left") # NA's will be filled in for short names that don't have a match in highertaxa now
    if (order == "dataframe") {
        return(ret_df)
    } else if (order == "unique_sp") {
        return(ret_df[-duplicated(ret_df$query),])
    }
}

format_sma_for_ggplot <- function(sma_mod, site_data, group = c("site", "other", "none"), group_vec, site_vec, ret_df = c("data", "fit", "all")) {
    # Formats for cross-site plotting in ggplot, assumes there is a "site" column in the data frame.  If there isn't, then read below...
    # assumes the SMA is performed with site as a grouping variable.  If it's different, then use group = "other",
    # and pass in the group/site vectors to match site data to.  group_vec and site_vec serve as lookups.  They must be same length, with elements corresponding to each other.
    # site_vec should be an ordered factor if you want site to be ordered in your plot
    # returns the dataframe with one row for each data point by default.  You can also choose to just return one row per fit group, or a list with both dataframes.
    library(gdata)
    library(plyr)
    group = match.arg(group)
    ret_df = match.arg(ret_df)
    sma_coefs = sma_mod$groupsummary
    if (group == "site") {
        sma_coefs$site = sma_coefs$group
        sma_coefs$site = reorder.factor(sma_coefs$site, new.order = levels(sma_mod$data$site))
    } else if (group == "other") {
        sma_coefs[,sma_mod$groupvarname] = sma_coefs$group
        sma_coefs$site = site_vec[match(sma_coefs$group, group_vec)]
        sma_coefs$site = reorder.factor(sma_coefs$site, new.order = levels(site_vec))
    } else if (group == "none") {                             # no grouping variable specified
        sma_mod$groupvarname = "group" # hack a groupvarname into the sma model for compatibility with the code below
        sma_mod$data$group = "all" # hack this in as well
        sma_coefs$site = NA
    }
    # add line segments for geom_segment
    sma_data = join(sma_coefs, sma_mod$data, by = sma_mod$groupvarname, type="right")
    x_var = names(sma_mod$data)[2]
    sma_data = ddply(sma_data, .(group), function(x) { x_min = min(x[,x_var], na.rm=T); x_max = max(x[,x_var],na.rm=T); return(cbind(x,x_min,x_max)) } )
    sma_data$y_min = sma_data$Int + sma_data$x_min * sma_data$Slope
    sma_data$y_max = sma_data$Int + sma_data$x_max * sma_data$Slope
    sma_data = join(sma_data, site_data, by = "site", type = "left")
    if (ret_df == "data") {
        return(sma_data)
    } else if (ret_df == "fit") {
        return(sma_data[! duplicated(sma_data$groupvarname),])
    } else if (ret_df == "all") {
        return(list(data = sma_data, fit = sma_data[! duplicated(sma_data[,sma_mod$groupvarname]),]))
    }
}

format_lmer_for_ggplot <- function(fit, labels = NA, n_main_effects = 1, match_site_data = TRUE, remove_main_effects_regex, rename_label_regex,
                                   type = c("all", "main", "interactions"), confint = c("profile", "Wald", "boot"), nsim = 200, site_factor = F,
                                   return_data = F) {
    # remove_main_effects_regex - vector of main effect names to remove from plot.  Matches the beginning of the label names, and exludes labels with ":".
    # rename - regex run against all labels via "sub()".  Renamed with first group match.
    # return_data - returns dataframe with site joined, and coords for geom_segment indicating fits
    # TODO: keep all factors, and return a column of the new names and those that should be removed in the plot.  Or something like that.  Maybe an attribute.

    library(plyr)
    library(dplyr)
    library(lazyeval)
    library(parallel)
    type = match.arg(type)
    confint = match.arg(confint)

    if (confint == "profile") {
        clust = makeCluster(detectCores() - 1)
        cis = as.data.frame(confint(fit, parm = "beta_", parallel = "snow", cl = clust, method = confint))
        stopCluster(clust)
    } else if (confint == "boot") {
        clust = makeCluster(detectCores() - 1)
        cis = as.data.frame(confint(fit, parm = "beta_", parallel = "snow", cl = clust, method = confint, nsim = nsim))
        stopCluster(clust)
    } else {
        cis = as.data.frame(confint(fit, parm = "beta_", method = confint))
    }

    cis$label = rownames(cis)
    rownames(cis) = NULL
    names(cis)[1:2] = c("lowCI","highCI")
    fe = data.frame(label = names(fixef(fit)), effect = fixef(fit), stringsAsFactors = F)
    rownames(fe) = NULL
    fe = join(fe, cis, by = "label")

    # rename common labels
    fe$label = sub("log10\\(DBH1/20\\)", "Stem Radius", fe$label)
    fe$label = sub("log10\\(stem_rad\\)", "Stem Radius", fe$label)
    fe$label = sub("log10\\(DBH1/10\\)", "Stem Diameter", fe$label)
    fe$label = sub("\\(Intercept\\)", "Intercept", fe$label)
    fe$label = sub("log10\\(h_tree\\)", "Tree Height", fe$label)

    if (!missing(remove_main_effects_regex)) {
        for (main_effect_regex in remove_main_effects_regex) {
            fe = subset(fe, ! grepl(paste0("^",main_effect_regex,"[^:]*$"), label))  # get rid of main effect rows if we're just interested in interactions.
        }
    }

    # rename labels
    if (!missing(rename_label_regex)) {
        for (rename_regex in rename_label_regex) {
            fe$label = sub(rename_regex, "\\1", fe$label)
        }
    }

    # rename labels based on their order passed in
    if (!all(is.na(labels))) {
        if(length(labels) < length(fe$label)) { labels[(length(labels)+1):length(fe$label)] = NA }
        fe[!is.na(labels),]$label = labels[!is.na(labels)]
    }

    if (match_site_data) {
        site_effects = grepl("site", fe$label) # won't match if site removed with "rename_label_regex
        fe[site_effects,]$label = sub("(.*?)[^:]*(\\w{3}\\d{2}.*)$","\\1\\2",fe[site_effects,]$label)  # remove prefix from label when something like a plot code found
        fe$site = fe$label
        fe$site = sub("^.*(\\w{3}\\d{2}).*$","\\1",fe$site) # remove prefix & suffix from site column when something like a plot code found
        fe = join_site(fe, site_factor = site_factor)
        levels(fe$region) = c(levels(fe$region), "All")
        fe$region[is.na(fe$region)] = "All"
        levels(fe$eco_class) = c(levels(fe$eco_class), "All")
        fe$eco_class[is.na(fe$eco_class)] = "All"
        site_levels = levels(model.frame(fit)$site)
        non_site_effects = fe$label[! fe$label %in% site_levels]
        fe$site = factor(fe$site, levels = c(non_site_effects,site_levels))
        fe$label = factor(fe$label, levels = fe$label) # order labels by the order of the data for now.  TODO: reorder based on plot orders.  Beware: plot orders can be duplicated if both intercept and slope terms included.
    }

    ret = fe

    if (return_data) {
        # add line segments for categorical predictors for geom_segment
        lmer_data = fit@frame
        if (any(names(lmer_data) %in% c("site", "plot", "plot_code"))) {
            site_col = names(lmer_data)[which(names(lmer_data) %in% c("site", "plot", "plot_code"))[1]]
        }
        lmer_data = join_site(lmer_data, site_factor = site_factor)
        cat_cont_list = attributes(getME(fit, "X"))[["contrasts"]]
        cat_preds = names(cat_cont_list)
        # for each level of categorial predictor, get x & y predicted segment endpoints
        lmer_preds = list()
        for (pred in cat_preds) {
            lmer_data_temp = cbind(lmer_data, rep(NA, nrow(lmer_data)))
            colname = paste0(pred,"pred")
            names(lmer_data_temp) = c(names(lmer_data), colname)
            lmer_data_temp[,colname] = predict(fit, re.form = NA)
            levels = rownames(cat_cont_list[[pred]])
            # get max and min predicted rows
            interaction_col = "log(DBH1/20)" # make this programmatic later

            lmer_data_temp = lmer_data_temp %>% group_by_(.dots = pred) %>%
                filter_(interp("predcol == max(predcol)", predcol = as.name(colname))) %>%
                rename_(interp("interaction_col = \"xmax\", predcol = \"ymax\"", interaction_col = as.name(interaction_col), predcol = as.name(colname)))


            lmer_data_temp_max = lmer_data_temp %>% group_by_(.dots = pred) %>%
                filter_(interp("predcol == max(predcol)", predcol = as.name(colname))) %>%
                rename_(interp("interaction_col = xmax, predcol = ymax", interaction_col = as.name(interaction_col), predcol = as.name(colname)))
            lmer_data_temp_min = lmer_data_temp %>% group_by_(.dots = pred) %>%
                filter_(interp("predcol == min(predcol)", predcol = as.name(colname))) %>%
                rename_(interp("interaction_col = xmin, predcol = ymin", interaction_col = as.name(interaction_col), predcol = as.name(colname)))
            lmer_data_temp = left_join(lmer_data_temp_max, select_(lmer_data_temp_min, .dots = c(pred, "xmin", "ymin")), by = pred)
            lmer_preds[[pred]] = lmer_data_temp
        }
        ret = list(fixed_effects = fe, cat_preds = lmer_preds)
    }

    return(ret)

}


make_lmer_geom_segments <- function(mod, groupvar = "site", xvar = "log10(DBH1/20)", return_data = T) {
    # return a dataframe with the group column and x/y variables suitable for use by ggplot2::geom_segment
    library(lazyeval)
    df = mod@frame
    df[,"y"] = predict(mod, re.form = NA)
    df = df %>% group_by_(.dots = groupvar) %>%
        filter(row_number(y) %in% c(1, n())) %>%
        arrange_(.dots = c(groupvar, "y")) %>%
        mutate_(x = interp(~ xvar, xvar = as.name(xvar)),
                xmax = interp(~ lead(xvar), xvar = as.name(xvar)),
                ymax = interp(~ lead(y), y = as.name("y"))) %>%
        filter(row_number(y) == 1)
    if (return_data) {
        ret = list(data = mod@frame, segments = df)
    } else {
        ret = df
    }
    return(ret)
}

add_main_effect_to_dev_contrasts <- function(df, main_effect) {
    # This function adds the mean of the main effect to the other effects fit with deviance contrasts.  This method is not rigorous:
    #   it does not incorporate the uncertainty of the estimate of the main effect, but hopefully does a good-enough job for figures.
    #   Not recommended to publish these values in tables - best to stick with model outputs for that.
    # To do this properly, you'd want to figure out the proper contrasts to estimate both overall effect and site effects.  Or,
    #   more likely, fit two models: one with deviance contrasts for overall site effect, another with treatment contrasts (maybe?)
    #   for per-site effects, and then merge those data into a single figure.
    # This function is meant to work with dataframes produced by format_lmer_for_ggplot, but will work with any dataframe with correct column names

    main_effect_mean = df[df$label == main_effect,]$effect
    df[! df$label %in% c("Intercept", main_effect),]$effect = df[! df$label %in% c("Intercept", main_effect),]$effect + main_effect_mean
    df[! df$label %in% c("Intercept", main_effect),]$lowCI = df[! df$label %in% c("Intercept", main_effect),]$lowCI + main_effect_mean
    df[! df$label %in% c("Intercept", main_effect),]$highCI = df[! df$label %in% c("Intercept", main_effect),]$highCI + main_effect_mean
    return(df)
}

clean_species_for_phylo <- function(binomial) {
    binomial = gsub("liana|cf|aff","",binomial,ignore.case=T) # remove the word liana and cf- often included in the binomial
    binomial = gsub("[^A-Za-z ]","",binomial) # remove non-alpha characters
    binomial = sub("\\s*(.*?)\\s*","\\1",binomial) # remove leading and trailing spaces
    binomial = gsub("\\s+"," ",binomial) # remove multiple spaces
    binomial = sub("^(\\w+\\s\\w+).*$","\\1",binomial) # finally, just keep the first two words
    return(binomial)
}

clean_species <- function(binomial) {
    # a more general name cleaner than the one above that is targeted to some gemtraits names that are dirty
    binomial = gsub("[^A-Za-z ]","",binomial) # remove non-alpha characters
    binomial = sub("\\s*(.*?)\\s*","\\1",binomial) # remove leading and trailing spaces
    binomial = gsub("\\s+"," ",binomial) # remove multiple spaces
    binomial = sub("^(\\w+\\s\\w+).*$","\\1",binomial) # finally, just keep the first two words
    return(binomial)
}

phylo_sp_to_string <- function(binomial) {
    # takes genus_species vector and returns Genus species
    binomial = sub("_"," ",binomial)
    return(paste0(toupper(substr(binomial, 1, 1)), substr(binomial, 2, length(binomial))))
}

match_phylo_df <- function(phylo, df, df_out = c("dataframe", "vector")) {
    # drops tips that don't match df rownames, and drops df rows that don't match phylo tips
    # df must have rownames consisting of the species
    # phylo must be phylo object
    # if df_out = vector, only the first column will be returned as a named vector
    # returns df/vector in the same order as the phylo tip labels
    df_out = match.arg(df_out)
    df = subset(df, rownames(df) %in% phylo$tip.label)
    phylo = drop.tip(phylo, which(! phylo$tip.label %in% rownames(df)))
    if (class(df) == "data.frame") {
        rownames = rownames(df)
        order = match(phylo$tip.label, rownames(df))
        df = df[order,,drop=FALSE]
        rownames(df) = rownames[order]
    } else {
        df = df[match(phylo$tip.label, names(df))]
    }
    if (df_out == "vector") {
        df = setNames(df[,1], rownames(df))
    }
    return(list(phylo = phylo, df = df))
}

remove_basal_taxa <- function(df, fam_col = "fp_family_name", taxa = c("all", "palms", "treeferns", "gymnosperms")) {
    taxa = match.arg(taxa)
    taxa_string = switch(taxa,
                         palms = "Arecaceae",
                         treeferns = "Cyatheaceae",
                         gymnosperms = "Araucariaceae|Boweniaceae|Cephalotaxaceae|Cupressaceae|Cycadaceae|Ephedraceae|Ginkgoaceae|Gnetaceae|Pinaceae|Podocarpaceae|Taxaceae|Taxodiaceae|Welwitschiaceae|Zamiaceae",
                         all = "Arecaceae|Cyatheaceae|Araucariaceae|Boweniaceae|Cephalotaxaceae|Cupressaceae|Cycadaceae|Ephedraceae|Ginkgoaceae|Gnetaceae|Pinaceae|Podocarpaceae|Taxaceae|Taxodiaceae|Welwitschiaceae|Zamiaceae")
    df = df[! grepl(taxa_string, df[,fam_col], ignore.case = T), ]
    return(df)
    # for genera, "Cyathea|Retrophyllum|Dicksonia|Alsophila|Podocarpus"
}

forestplot_dump_import <- function(this_file) {
    # file should be named with 3 letter + 2 digit plot code

    library("XLConnect")
    this_plot_wb <- loadWorkbook(this_file)
    this_plot = readWorksheet(this_plot_wb, "Plot Dump", startRow = 2)
    this_plot$Height = as.numeric(this_plot$Height)
    this_plot$plot = sub("^.*([a-zA-Z]{3})-?(\\d\\d).*", "\\1-\\2", this_file) #Assume file is named with plot to start

    # For now, just import the last census.  All cols through "WD Type", and then from the last occurrence of DBH0 to the end.
    this_plot_last_cols = this_plot[,max(grep("DBH0", names(this_plot))):ncol(this_plot)]
    this_plot_first_cols = this_plot[,1:grep("WD.Type", names(this_plot))]
    this_plot = cbind(this_plot_first_cols, this_plot_last_cols)
    # clean col names
    names(this_plot) = gsub("(.*?)\\.\\d*", "\\1", names(this_plot))

    this_plot$DBH0 = as.numeric(this_plot$DBH0)
    this_plot$DBH1 = as.numeric(this_plot$DBH1)

    this_plot$BA = pi * (this_plot$DBH1/10/2)^2
    this_plot$treecode = paste(gsub("-","",this_plot$plot), "-T", this_plot$TagNo, sep="") # make treecode compatible with chambasa coding in order to join
    # TODO: deal with "I" trees (out of plot, and lianas)

    this_plot$Height = as.numeric(this_plot$Height)
    return(this_plot)
}

make_latex_safe <- function(str) {
    # Substitute special characters to make a string print as is in latex
    str = gsub("\\","\\textbackslash{}", str, fixed=TRUE)
    str = gsub("(?=[#$%&_{}])","\\\\",str,perl=T)
    str = gsub("~", "\\\\~{}", str)
    str = gsub("\\^","\\\\^{}", str)
    return(str)
}

myround <- function(x, digits=1) {
    # from https://github.com/kbroman/broman/blob/master/R/myround.R
    # Round a number, preserving extra 0's
    if(digits < 1)
        stop("This is intended for the case digits >= 1.")

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)

    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero

    tmp
}

species_mean_trait <- function(species, trait, return_all_species = T) {
    library(plyr)
    trait_df = data.frame(species = species, trait = trait)
    na.omit(trait_df)
    species_mean = ddply(trait_df, .(species), summarize, mean = mean(trait, na.rm=T), nobs = sum(!is.na(trait)))
    species_mean$genus = sub("(\\w) .*","\\1", species_mean$species)
    if( any(species_mean$nobs == 0) ) {
        species_mean[species_mean$nobs == 0,]$mean = NA
    }
    if (! return_all_species) {
        species_mean = na.omit(species_mean)
    }
    genus_mean = ddply(species_mean, .(genus), summarize, sp_nobs = sum(!is.na(mean)), mean = mean(mean, na.rm=T), ind_nobs = sum(nobs, na.rm=T))
    if ( any(genus_mean$ind_nobs == 0) ) {
        genus_mean[genus_mean$ind_nobs == 0,]$mean = NA
    }
    species_mean$genus = NULL
    genus_mean = genus_mean[,c(1,3,2,4)]
    return(list("species_mean" = species_mean, "genus_mean" = genus_mean))
}

fill_trait_by_species <- function(trait_df, species_mean) {
    # species_mean is a dataframe as returned by species_mean_trait() above
    # first arg is a dataframe with 2 columns - species and traits
    species = clean_species(trait_df[,1])
    trait = trait_df[,2]
    trait_missing = is.na(trait)
    trait[trait_missing] = species_mean[match(species[trait_missing], clean_species(species_mean$species)),]$mean
    trait_match = NA
    trait_match[trait_missing & !is.na(trait)] = "species"
    trait_df[,2] = trait
    trait_df$trait_match = trait_match
    return(trait_df)
}

fill_trait_by_genus <- function(trait_df, genus_mean) {
    # genus_mean is a dataframe as returned by species_mean_trait() or fill_trait_by_species() above.  It should have at least two columns: genus and mean
    # first arg is a dataframe with 2 or 3 columns - species, traits, and optionally a "matched" column
    species = clean_species(trait_df[,1])
    genus = sub("^(\\w+) .*$","\\1",species)
    trait = trait_df[,2]
    trait_missing = is.na(trait)
    #genus_mean_genus = genus_mean$genus
    trait[trait_missing] = genus_mean[match(genus[trait_missing], genus_mean$genus),]$mean
    if (ncol(trait_df) >= 3) {
        trait_match = trait_df[,3]
    } else {
        trait_match = NA
    }

    #     if ("trait_match" %in% colnames(trait_df)) {
    #         trait_match = trait_df$trait_match
    #     } else {
    #         trait_match = NA
    #     }

    trait_match[trait_missing & !is.na(trait)] = "genus"
    trait_df[,2] = trait
    trait_df$trait_match = trait_match
    return(trait_df)
}

contr.sum.keepnames <- function(...) {
    # make deviation contrasts that don't lose the names of the factors in the model results
    # from https://stackoverflow.com/questions/10808853/why-does-changing-contrast-type-change-row-labels-in-r-lm-summary
    conS <- contr.sum(...)
    colnames(conS) = rownames(conS)[-length(rownames(conS))]
    conS
}

set_dev_contrasts <- function(df, colname = "site") {
    # Set contrasts to "deviation coding" so site effects are as compared to overall mean across all sites.  I.e., sites together should have a mean 0 effect.
    # See http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm
    col2setup = df[,colname]
    col2setup = as.factor(col2setup)[, drop = TRUE]
    contrasts(col2setup) <- contr.sum.keepnames(levels(col2setup))
    df[,colname] = col2setup
    return(df)
}

normalize_cols <- function(df, cols2norm, sun_shade2numeric = T) {
    library(caret)
    if (sun_shade2numeric) {
        if ("sun_shade" %in% colnames(df)) {
            df$sun_shade = as.numeric(as.factor(df$sun_shade))
        }
    }

    if (!missing(cols2norm)) {
        dfout = subset(df, select = cols2norm)
    } else {
        numeric_cols = sapply(df, is.numeric)
        non_numeric_cols = ! sapply(df, is.numeric)
        # note - do we want to test for normality within groups, such as within sites?
        # answer - actually, we want to test model RESIDUALS for normality...  Need to implement this...
        dfout = df[,numeric_cols]
    }

    # Scale and center everything to start, no matter what
    temp <- data.frame(lapply(dfout, scale))
    rownames(temp) = rownames(dfout)
    dfout = temp

    non_normal_cols = names(dfout)[lapply(lapply(dfout, shapiro.test), "[[", "p.value") < 0.05]
    dfout[,non_normal_cols] = predict(preProcess(dfout[,non_normal_cols], method = c("BoxCox", "scale", "center")), dfout[,non_normal_cols])
    non_normal_cols = names(dfout)[lapply(lapply(dfout, shapiro.test), "[[", "p.value") < 0.05]
    if (length(non_normal_cols) > 0) {
        warning("Some columns still not normal: ", paste(non_normal_cols, collapse = "; "), ". Attempting 2-parameter BoxCox...")
    }

    for (col in non_normal_cols) {
        # try two-parameter box-cox - http://robjhyndman.com/hyndsight/transformations/
        data = dfout[,col]
        data = data + min(data, na.rm = T)
        fit <- tryCatch(
            boxcoxfit(na.omit(data), lambda2 = T, na.rm = T),
            error = function(e) e
        )

        if (!inherits(fit, "error")) {
            lambda = fit[["lambda"]][1]
            lambda2 = fit[["lambda"]][2]
            datatrans = ( (data + lambda2)^lambda - 1 ) / lambda
            datatrans = scale(datatrans) # scale and center the transformed data
            if (is.null(attributes(datatrans)))
                attrs = list(xform = "2-parm boxcox", lambda = lambda, lambda2 = lambda2) else
                    attrs = c(attributes(datatrans), list(xform = "2-parm boxcox", lambda = lambda, lambda2 = lambda2))
            attributes(datatrans) = attrs
            dfout[,col] = datatrans
        } else {
            # Just center and scale if nothing else works
            dfout[,col] = scale(dfout[,col])
        }
    }

    non_normal_cols = names(dfout)[lapply(lapply(dfout, shapiro.test), "[[", "p.value") < 0.05]
    if (length(non_normal_cols) > 0) {
        warning("Some columns still not normal after 2-parameter xform: ", paste(non_normal_cols, collapse = "; "))
    }

    if (!missing(cols2norm)) {
        dfout2 = df
        dfout2[,cols2norm] = dfout
    } else {
        dfout2 = df
        dfout2[,numeric_cols] = dfout
    }

    return(dfout2)
}

cowplot_labels <- function(len) {
    ret = letters[1:len]
    ret = sub("(.*)","\\(\\1\\)",ret)
    return(ret)
}

