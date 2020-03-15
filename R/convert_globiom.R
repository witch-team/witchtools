
#' @importFrom stats approx
# Main function to convert GLOBIOM DATA

convert_globiom <- function(gbfile,
                            reg_id,
                            time_id,
                            region_mappings,
                            region_definitions,
                            time_mappings,
                            weights,
                            output_directory){

  if (!file.exists(gbfile)) stop(paste(gbfile, "does not exist!"))

  cat(crayon::blue$bold(paste("Processing", basename(gbfile), "\n")))

  sqldb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = gbfile)
  .globiom <- data.table::setDT(RSQLite::dbGetQuery(sqldb, 'select * from globiom'))
  RSQLite::dbDisconnect(sqldb)

  # Region conversion
  input_reg_id <- intersect(colnames(.globiom), names(region_definitions))
  param_agg <- ""

  .globiom[[which(input_reg_id == colnames(.globiom))]] <- tolower(.globiom[,get(input_reg_id)])

  meta_param <- data.table::fread('parameter,type,value
  co2def,nweight,forest
  Q_PES_WBIO,nweight,wbio_2010
  co2aff,nweight,forest
  co2fmg,nweight,forest
  ch4_agr,nweight,ch4lu_emissions_2005
  n2o_agr,nweight,n2olu_emissions_2005
  land_crop,nweight,agland
  land_crop_cereal,nweight,agland
  land_crop_energy,nweight,agland
  land_crop_irrigated,nweight,agland
  land_grass,nweight,agland
  land_forest,nweight,forest
  prod_cereal,nweight,agland
  prod_energy,nweight,agland
  prod_livestock,nweight,agland
  calorie_animal,nagg,mean
  calorie_animal,nweight,pop
  calorie_crop,nagg,mean
  calorie_crop,nweight,pop
  irri_withdrawal,nweight,agland
  food_price,nagg,mean
  food_price,nagg,gdp
  crop_price,nagg,mean
  crop_price,nagg,gdp
  Avoided_CO2,nweight,co2
  Cost_REDD,nweight,co2')

  items <- colnames(.globiom)[!colnames(.globiom) %in% c(input_reg_id,"ssp","bio_price","co2_price","year")]
  params <- list()

  for (item in items) {

    .data <- .globiom[,c(input_reg_id,"ssp","bio_price","co2_price","year",item),with = FALSE]
    data.table::setnames(.data,item,"value")
    .data[is.na(value),value := 1e-7]
    .data[abs(value) < 1e-7 & value >= 0,value := 1e-7]
    .data[abs(value) < 1e-7 & value < 0,value := -1e-7]
    if (item %in% c("Avoided_CO2","Cost_REDD")) {
      .data <- .data[,.(value = mean(value)),by = c(input_reg_id,"ssp","co2_price","year")]
      .data[,bio_price := NA]
    }

    # Select aggregation type (sum or mean)
    find_w <- meta_param[parameter == item & type == "nagg"]
    if (nrow(find_w) > 0) {
      param_agg <- find_w[1,value]
    }else{
      param_agg <- "sum"
    }

    # Select weighting
    find_w <- meta_param[parameter == item & type == "nweight"]
    if (nrow(find_w) > 0) {
      param_w <- find_w[1,value]
    }else{
      param_w <- ifelse(param_agg == "mean","cst","gdp")
    }

    if (input_reg_id != reg_id) {

      .r <- merge(region_mappings[[input_reg_id]],region_mappings[[reg_id]], by = "iso3")
      .data <- merge(.data, .r, by = input_reg_id, allow.cartesian = TRUE)

      cat(crayon::blue(paste0(" - parameter ", item, " [agg: ", param_agg, ", wgt: ", param_w, "]\n")))

      # Add weight
      .data <- merge(.data, weights[[param_w]], by = "iso3")
      .data <- .data[!is.na(get(reg_id))]

      dkeys <- function(dd){
        return(c(colnames(dd)[!colnames(dd) %in% c("value","weight","sum_weight",names(region_definitions))]))
      }

      # Disaggregation
      if (param_agg %in% c("sum")) {
        # total weights are computed because of missing zeros values
        .w <- merge(region_mappings[[input_reg_id]],weights[[param_w]],by = "iso3")
        .w <- .w[iso3 %in% unique(.data$iso3)]
        .w <- .w[,.(sum_weight = sum(weight)),by = input_reg_id]
        .data <- merge(.data,.w,by = input_reg_id)
        .data <- .data[, .(iso3,reg_id = get(reg_id),value = value * weight / sum_weight), by = c(dkeys(.data),input_reg_id) ]
      } else {
        if (param_agg == "mean") {
          .data <- .data[, .(iso3,reg_id = get(reg_id),value = value,weight), by = c(dkeys(.data),input_reg_id) ]
        } else if (param_agg == "set1") {
          .data <- .data[, .(iso3,reg_id = get(reg_id),value = value,weight), by = c(dkeys(.data),input_reg_id) ]
        } else if (param_agg %in% c("min","max")) {
          .data <- .data[, .(iso3,reg_id = get(reg_id),value = value,weight), by = c(dkeys(.data),input_reg_id) ]
        } else {
          stop(paste("Disaggregation",param_agg,"not implemented"))
        }
      }

      # Aggregation
      if (param_agg %in% c("sum","sumby")) {
        .data <- .data[, .(value = sum(value)), by = c(dkeys(.data)) ]
      } else {
        .w <- merge(region_mappings[[reg_id]],weights[[param_w]],by = "iso3")
        .w <- .w[,.(sum_weight = sum(weight)),by = reg_id]
        data.table::setnames(.w, reg_id, "reg_id")
        .data <- merge(.data,.w,by = "reg_id")
        if (param_agg == "mean") {
          .data <- .data[, .(value = sum(value * weight / sum_weight)), by = c(dkeys(.data)) ]
        } else if (param_agg == "set1") {
          .data <- .data[, .(value = round(sum(value * weight / sum_weight))), by = c(dkeys(.data)) ]
        } else if (param_agg == "min") {
          .data <- .data[, .(value = min(value[which(weight == min(weight))])), by = c(dkeys(.data)) ]
        } else if (param_agg == "max") {
          .data <- .data[, .(value = max(value[which(weight == max(weight))])), by = c(dkeys(.data)) ]
        }  else {
          stop(paste("aggregation",param_agg,"not implemented"))
        }
      }

      # Change the region column name
      data.table::setnames(.data, "reg_id", "n")

    } else {

      cat(crayon::blue(paste(" - parameter", item, "[same]\n")))

      # Change the region column name and indices
      data.table::setnames(.data, reg_id, "n")

    }

    # interpolate
    .data[,year := as.numeric(year)]
    years <- as.numeric(unique(time_mappings[[time_id]]$refyear))
    .data <- .data[,.(year = years,
                     value = approx(x = .SD$year, y = value, xout = years, rule = 2)$y),
                  by = c(colnames(.data)[!names(.data) %in% c("value","year")])]

    # Map year - periods
    .data <- merge(.data, time_mappings[[time_id]][,.(t,year)], by = "year", allow.cartesian = TRUE)
    .data[, year := NULL]

    .data$var <- item

    params <- c(params, list(.data))

  }

  params <- data.table::rbindlist(params,use.names = TRUE)

  cat(crayon::blue(paste(" -","writing db\n")))

  sqldb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = file.path(output_directory,basename(gbfile)))
  RSQLite::dbWriteTable(sqldb, "globiom", params, row.names = FALSE, overwrite = TRUE,
               append = FALSE, field.types = NULL)
  RSQLite::dbDisconnect(sqldb)

}

