summarise_hyps <- function(hyps, shoreline) {
  # if (!is.null(aeme)) {
  #   inp <- AEME::input(aeme)
  #   hyps <- inp$hypsograph |> 
  #     dplyr::filter(depth <= 0)
  # }
  
  lm <- lakemorpho::lakeMorphoClass(shoreline)
  sdi <- lakemorpho::lakeShorelineDevelopment(lm) |> round(2)
  shore_length <- (lakemorpho::lakeShorelineLength(lm) / 1000 ) |> 
    round(1)
  
  df <- hyps |> 
    dplyr::summarise(Z_max = max(abs(depth)),
                     surf_area = max(area),
                     area_Ha = max(area) / 1e4,
                     lake_elev = max(elev),
                     V = calc_V(max(elev), elev, area),
                     Z_mean = V / surf_area,
                     DV = (3 * Z_mean) / Z_max,
                     sdi = sdi,
                     shore_length) |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) round(x, 2))) 
  
}

calc_V <- function (depth, elev, area, h = 0.1) {
  sapply(depth, function(d) {
    depths <- seq(min(elev), d, h)
    if (tail(depths, 1) != d) {
      depths <- c(depths, d)
    }
    areas <- approx(elev, area, depths)$y
    r <- sqrt((c(areas[-length(areas)])/pi))
    R <- sqrt((areas[-1]/pi))
    V <- ((pi * h)/3) * (R * R + R * r + r * r)
    sum(V)
  })
}

