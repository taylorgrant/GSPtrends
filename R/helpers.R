#' Split a string into a vector of strings
#'
#' @param string A comma separated string
#'
#' @return A vector of strings based on comma locations
#' @export
#'
#' @examples
#' bare_combine("t1, t2, t3, t4")
bare_combine <- function(string) {
  bits <- unlist(strsplit(string, ",", " "))
  bits <- stringr::str_trim(bits)
}


palette_choice <- function(palette) {
  red.pal <- c("#7e100e", "#872a14", "#8f3e1c", "#975026", "#9f6132", "#a77240",
               "#ae8250", "#b69360", "#bda372", "#c5b386", "#cec39a",
               "#d8d2ae", "#e2e2c4")
  bty.pal <- c("#2a4858", "#255667", "#1a6575", "#057480", "#008488", "#00938d",
               "#12a28f", "#32b18e", "#50bf8b", "#6ecd85", "#8eda7f",
               "#b0e678", "#d4f171")
  blue.pal <- c("#1f456e", "#24527b", "#2b6087", "#326d93", "#3a7b9f", "#4389ab",
                "#4d98b6", "#58a6c1", "#64b5cc", "#71c3d6", "#7fd2e1",
                "#8ee1eb", "#9df0f5")
  purple.pal <- c("#752092", "#91238c", "#a72b86", "#ba3781", "#c9477c", "#d65779",
                  "#e06977", "#e87b77", "#ee8d7a", "#f39f80", "#f6b089",
                  "#f9c294", "#fcd3a2")
  green.pal <- c("#081c15", "#0f3526", "#174e37", "#1e6845", "#268153", "#2d9a5f",
                 "#35b269", "#41c774", "#5bce82", "#74d591", "#8ddca2",
                 "#a6e4b4", "#bfebc7")
  solr.pal <- c("#03071e", "#17152f", "#2c1d3f", "#45244c", "#612956", "#7d2f5c",
                "#99355e", "#b43e5d", "#cc4a57", "#e05b4f", "#f06f44",
                "#fb8635", "#ff9f24")
  pink.pal <- c("#ff0a54", "#ff345e", "#ff4b68", "#ff5d73", "#ff6c7d", "#ff7b88",
                "#ff8993", "#ff969e", "#ffa3a9", "#ffb0b5", "#ffbcc0",
                "#ffc8cc", "#fed4d8")
  if (palette == "red")
    return(red.pal)
  else if (palette == "bty")
    return(bty.pal)
  else if (palette == "blue")
    return(blue.pal)
  else if (palette == "purple")
    return(purple.pal)
  else if (palette == "solr")
    return(solr.pal)
  else if (palette == "green")
    return(green.pal)
  else if (palette == "pink")
    return(pink.pal)
  # else if (palette == "bly")
  #   return(bly.palette)
  # else if (palette == "solr")
  #   return(solr.palette)
  # else if (palette == "btr")
  #   return(btr.palette)
  # else if (palette == "xf")
  #   return(xf.palette)
}
