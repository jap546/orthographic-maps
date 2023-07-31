library(terra)
library(sf)
library(giscoR)
library(fs)
library(tidyverse)
library(ggshadow)
library(ggforce)

plot_theme = "sea_ice"

folder = glue::glue("data/{plot_theme}/copernicus")

ortho_crs ='+proj=ortho +lat_0=65 +lon_0=20 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

world_poly = gisco_get_countries(year = "2020", epsg = "4326", resolution = "10") 

# get the global graticule
grid = st_graticule()

# define what would be ocean
ocean = st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # earth radius
  st_sfc(crs = ortho_crs)


world = world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) 


grid_crp = st_difference(grid, st_union(world_poly))

# select the visible part
grid_crp = st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)

ocean_df = st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

median_ice_extent = read_sf("data/sea_ice/median_extent_N_09_1981-2010_polyline_v3.0.shp") |>
  st_transform(ortho_crs)


year = "2022"

fpaths = dir_ls(glue::glue(path(folder, "mds", {year})), regexp = "nc") 


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

ice_cov_data = read_rast(fpaths[9], "siconc")
ice_thick_data = read_rast(fpaths[9], "sithick")



sea_ice_plot = function(rast_df, var, colour_pal, breaks, title) {
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
    geom_sf(
      data = ocean,
      fill = "grey80",
      colour = NA) +
    geom_sf(
      data = grid_crp, 
      colour = "white", 
      linewidth = 0.2) +
    geom_sf(
      data = world, 
      fill = "grey50",
      colour = "black",
      linewidth = 0.2) +
    geom_tile(
      data = rast_df, 
      aes(x, y, fill = !!var)
    ) +
    geom_sf(
      data = median_ice_extent,
      fill = NA,
      colour = "red",
      linewidth = 0.7,
      show.legend = "line" 
    )
  
  g2 = g2 + scale_fill_distiller(
    palette = colour_pal,
    limits = c(min(breaks),
               max(breaks)),
    breaks = breaks
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


# classInt::classify_intervals(ice_2017_df$ta, 4, style = "quantile")


sea_ice_plot(
  ice_cov_data$r_df,
  ta,
  "Blues",
  c(0, 20, 40, 60, 80, 100),
  glue::glue("Sea ice cover (%) - September {year}")
  )

sea_ice_plot(
  ice_thick_data$r_df,
  ta,
  "Blues",
  c(0, 1, 2, 3, 4, 5),
  glue::glue("Sea ice thickness (m) - September {year}")
)
