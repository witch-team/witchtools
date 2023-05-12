#' Many color palettes
#'
#' Creates thematic palettes for WITCH results and other
#'
#' @param component palette's component (`fuel`, `region`).
#' @param theme A palette theme (`witch-plot` (default)).
#' @param restrict_names only return the corresponding named values.
#' @export
#'
many_pals <- function(component = NULL, variant = NULL, theme = NULL, include_names = NULL) {

  # Few tests on function parameters
  if (is.null(component)) {
    warning('component is NULL')
    return(NULL)
  }

  all.components <- c("fuel","region")

  if (!component %in% all.components) {
    warning(paste('component should be chosen among',
                  paste(all.components, collapse = ", ")))
    return(NULL)
  }


  if (is.null(theme)) {
    theme <- "witch-plot"
  }

  if (component == "region") {

    if (theme == "witch-plot") {

      region_palette_specific <- setNames(rainbow(length(witch_regions)), witch_regions) #just in case have a fall back colour
      region_palette_witch <- c(usa="darkblue",Usa="darkblue",oldeuro="blue", neweuro="cornflowerblue",kosau="darkgreen",Kosau="darkgreen",cajaz="chartreuse4",Cajaz="chartreuse4",te="gold2",Te="gold2",mena="darkgoldenrod4",Mena="darkgoldenrod4",ssa="goldenrod",Ssa="goldenrod",sasia="darkorange2","South Asia"="darkorange2",china="deeppink3",PRC="deeppink3",easia="orangered",ESEAP="orangered",laca="#fbb714",Laca="#fbb714",india="#fbf003",India="#fbf003",europe="blue",Europe="blue",indonesia="lightsalmon3",Indonesia="lightsalmon3",Rest_of_World="grey48",chinaw="darkorange",chinac="darkorange2",chinae="darkorange4",italy="green",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="darkseagreen",oceania="forestgreen",southafrica="indianred3",seasia="orangered",World="black", "Global Pool"="black")
      #add ed57 region colors for RICE50+
      region_palette_ed57 <- c("arg" =  "#000000","aus" =  "#48d1cc","aut" =  "#ae8000","bel" =  "#800000","bgr" =  "#003366","blt" =  "#bf4040","bra" =  "#ffd633","can" =  "#6600cc","chl" =  "#ffece6","chn" =  "#ff531a","cor" =  "#adebad","cro" =  "#808080","dnk" =  "#ff9933","egy" =  "#0044cc","esp" =  "#ffd6cc","fin" =  "#00cccc","fra" =  "#cc0000","gbr" =  "#ffffdd","golf57"  =  "#33d6ff","grc" =  "#00ffcc","hun" =  "#9999ff","idn" =  "#996633","irl" =  "#ff4dff","ita" =  "#ffff00","jpn" =  "#006600","meme"=  "#b32d00","mex" =  "#ccff33","mys" =  "#145252","nde" =  "#00d900","nld" =  "#c309bd","noan"=  "#ffff99","noap"=  "#ecf2f9","nor" =  "#ff3399","oeu" =  "#ffb3ff","osea"=  "#008fb3","pol" =  "#d6f5d6","prt" =  "#003300","rcam"=  "#4d1919","rcz" =  "#00ffff","rfa" =  "#deb887","ris" =  "#000080","rjan57"  =  "#bf00ff","rom" =  "#ff00ff","rsaf"=  "#ff8000","rsam"=  "#0000ff","rsas"=  "#ccd6dd","rsl" =  "#00ff00","rus" =  "#66757f","slo" =  "#ff3091","sui" =  "#61a62f","swe" =  "#cb1942","tha" =  "#efff14","tur" =  "#4b0082","ukr" =  "#c198ff","usa" =  "#ffcc00","vnm" =  "#3377ff","zaf" =  "#b3ccff")
      #Add witch34 region colors
      region_palette_witch34 <- c("bnl" =  "#800000","northeu" =  "#bf4040","balkan" =  "#808080","easteu" =  "#9999ff", "che"="#61a62f", "deu" =  "#deb887", "rou" =  "#ff00ff", "cze" =  "#00ffff")
      region_palette <- replace(region_palette_specific, names(region_palette_witch), region_palette_witch)
      region_palette <- replace(region_palette, names(region_palette_ed57), region_palette_ed57)
      region_palette <- replace(region_palette, names(region_palette_witch34), region_palette_witch34)
      return(region_palette[[include_names]])

    }
  }

  if (component == "fuel") {
    default_cols <- c(coal = "#3e3e3e",
                      ngas  = "#659AC5",
                      nuclear = "#8E61E8",
                      oil = "#663E28",
                      solar = "#FFE205",
                      wind = "#252C8F")
    if (is.null(include_names)) {
      return(default_cols)
    }

    # select names from include_names
    cols <- default_cols[[include_names]]

    # Check similar names
    if (c("gas","natural_gas") %in% include_names) {
      #cols = c(cols, )
    }

    return(pal)
  }

}
