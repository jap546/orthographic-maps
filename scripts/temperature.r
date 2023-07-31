library(terra)
library(sf)
library(giscoR)
library(fs)
library(tidyverse)
library(ggshadow)
library(ggforce)

plot_theme = "temperature"

folder = glue::glue("data/{plot_theme}/copernicus")

ortho_crs = '+proj=ortho +lat_0=51 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

world_poly = gisco_get_countries(year = "2020", epsg = "4326", resolution = "10") 

# get the global graticule
grid = st_graticule()

# define what would be ocean
ocean = st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # earth radius
  st_sfc(crs = ortho_crs)


world = world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) # 


grid_crp = st_difference(grid, st_union(world_poly))

grid_crp = st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)

ocean_df = st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

# The formula to convert Kelvin into Celsius is C = K - 273.15.
# All that is needed to convert Kelvin to Celsius is one simple step:
# Take your Kelvin temperature and subtract 273.15. Your answer will be in Celsius.


fpaths = dir_ls(path(folder), regexp = "nc")

read_rast = function(fpath, var){
  
  r = rast(fpath)
  
  r = switch(var,
             "siconc" = r[[1]], # sea ice cover
             "sithick" = r[[2]], # sea ice thickness
             "temperature" = mean(r), # temperature
             # add more cases for additional layers
             NA)
  
  r = project(r, ortho_crs)
  
  r_df = as.data.frame(r, xy = TRUE) |>
    rename("date" = 3)
  
  if (var == "siconc") {
    r_df = r_df |>
      pivot_longer(3:length(r_df), names_to = "date", values_to = "ta") |>
      mutate(ta = ta * 100) |>
      filter(ta > 0)
  } else if (var == "sithick") {
    r_df = r_df |>
      pivot_longer(3:length(r_df), names_to = "date", values_to = "ta") |>
      mutate(ta = case_when(ta > 5 ~ 5, TRUE ~ ta)) |>
      filter(ta > 0)
  } else if (var == "temperature") {
    r_df = r_df |>
      pivot_longer(3:length(r_df), names_to = "date", values_to = "ta") |>
      mutate(ta = ta - 273.15) |> # calculate Celsius temperature
      mutate(ta_limit = case_when(ta > 40 ~ 40,
                                  ta < -12 ~ -12,
                                  TRUE ~ ta))
  }
  
  return(list(r = r, r_df = r_df))
}

temperature = read_rast(fpaths, "temperature")

temperature_plot = function(rast_df, var, colour_pal, breaks, title) {
  var = rlang::enquo(var)
  
  g = ggplot() +
    geom_glowpath(
      data = ocean_df,
      aes(X, Y, group = "L1"),
      shadowcolor = 'grey90',
      colour = "white",
      alpha = 0.01,
      shadowalpha = 0.05,
      shadowsize = 1.8
    ) +
    geom_glowpath(
      data = ocean_df,
      aes(X, Y, group = "L1"),
      shadowcolor = 'grey90',
      colour = "white",
      alpha = 0.01,
      shadowalpha = 0.02,
      shadowsize = 1
    ) +
    geom_glowpath(
      data = ocean_df,
      aes(X, Y, group = "L1"),
      shadowcolor = 'grey90',
      colour = "white",
      alpha = 0.01,
      shadowalpha = 0.01,
      shadowsize = 0.5
    )
  
  g2 = g +
    geom_tile(data = rast_df,
              aes(x, y, fill = !!var)) +
    geom_sf(data = grid_crp,
            colour = "white",
            linewidth = 0.2) +
    geom_sf(
      data = world,
      fill = NA,
      colour = "grey10",
      linewidth = 0.2
    )
  
  g2 + scale_fill_distiller(
    palette = colour_pal,
    limits = c(min(breaks),
               max(breaks)),
    breaks = breaks,
    labels = c(breaks[1:length(breaks) - 1],
               glue::glue(">{max(breaks)}"))
  ) +
    guides(fill = guide_colourbar(
      barwidth = 30,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5
    )) +
    coord_sf() +
    labs(fill = str_wrap(title, 50)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 15),
      plot.margin = margin(15, 15, 15, 15)
    ) 
  
  ggsave(
    filename = glue::glue("plots/{plot_theme}/{title}.png"),
    width = 15,
    height = 15,
    device = "png",
    dpi = 1000,
    bg = "white"
  )
}

# Determine break intervals if needed
# classInt::classify_intervals(temperature$r_df$ta_limit, 4, style = "quantile")

temperature_plot(temperature$r_df,
                 ta_limit,
                 "RdBu",
                 c(-13, 0, 13, 26, 40),
                 "Average temperature for June 2023")


