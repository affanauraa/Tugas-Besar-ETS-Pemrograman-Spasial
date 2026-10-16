
# =====================================================
# MEMBUAT PNG UNTUK WEBGIS LEAFLET PURWOREJO
# =====================================================

library(terra)

setwd("C:/Users/user/Downloads/TUBES ETS PEMSPA")

# Folder www dapat dibaca langsung oleh aplikasi Shiny
dir.create(
  "www/layers",
  recursive = TRUE,
  showWarnings = FALSE
)

# Mengubah koordinat Web Mercator ke longitude-latitude
mercator_ke_lonlat <- function(x, y) {
  R <- 6378137
  lon <- x / R * 180 / pi
  lat <- (2 * atan(exp(y / R)) - pi / 2) * 180 / pi
  c(lon = lon, lat = lat)
}

# Fungsi menghasilkan PNG dari raster
buat_png <- function(nama, kategori = FALSE) {
  
  # Membaca hasil prepare_leaflet.R
  r <- terra::rast(
    paste0("output/leaflet/", nama, "_web.tif")
  )
  
  # Memastikan satu layer
  stopifnot(terra::nlyr(r) == 1)
  
  # Membaca matriks piksel
  m <- terra::as.matrix(r, wide = TRUE)
  
  # Membuat warna untuk setiap sel
  warna <- matrix(
    "#00000000",
    nrow = nrow(m),
    ncol = ncol(m)
  )
  
  valid <- !is.na(m) & is.finite(m)
  
  if (kategori) {
    
    kode <- round(m)
    
    warna[valid & kode == 1] <- "#329447"
    warna[valid & kode == 2] <- "#D96050"
    warna[valid & kode == 3] <- "#3589CD"
    warna[valid & kode == 4] <- "#D9D9D9"
    
  } else {
    
    pal <- switch(
      nama,
      ndvi = c("#A50026", "#FFFFBF", "#006837"),
      ndbi = c("#FFFFCC", "#FD8D3C", "#800026"),
      ndwi = c("#F7FBFF", "#6BAED6", "#08306B"),
      ndci = c("#440154", "#21918C", "#FDE725")
    )
    
    warna_pal <- grDevices::colorRampPalette(pal)(256)
    
    nilai <- pmax(-1, pmin(1, m[valid]))
    indeks <- floor((nilai + 1) / 2 * 255) + 1
    
    warna[valid] <- warna_pal[indeks]
  }
  
  # Menulis PNG transparan tanpa sumbu dan margin
  file_png <- paste0("www/layers/", nama, ".png")
  
  grDevices::png(
    filename = file_png,
    width = ncol(m),
    height = nrow(m),
    bg = "transparent"
  )
  
  grid::grid.newpage()
  
  grid::grid.raster(
    grDevices::as.raster(warna),
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"),
    interpolate = FALSE
  )
  
  grDevices::dev.off()
  
  # Mengambil extent raster EPSG:3857
  e <- terra::ext(r)
  
  barat_selatan <- mercator_ke_lonlat(
    terra::xmin(e),
    terra::ymin(e)
  )
  
  timur_utara <- mercator_ke_lonlat(
    terra::xmax(e),
    terra::ymax(e)
  )
  
  # Batas untuk Leaflet:
  # sudut barat daya, lalu timur laut
  batas <- list(
    c(barat_selatan["lat"], barat_selatan["lon"]),
    c(timur_utara["lat"], timur_utara["lon"])
  )
  
  cat("SELESAI:", file_png, "\n")
  
  return(batas)
}

# =====================================================
# MENGHASILKAN LIMA PNG
# =====================================================

batas_layer <- list(
  klasifikasi = buat_png("klasifikasi", kategori = TRUE),
  ndvi = buat_png("ndvi"),
  ndbi = buat_png("ndbi"),
  ndwi = buat_png("ndwi"),
  ndci = buat_png("ndci")
)

# Menyimpan batas geografis setiap PNG
saveRDS(
  batas_layer,
  "www/layers/batas_layer.rds"
)

cat("\nSEMUA PNG SELESAI DIBUAT!\n")
print(list.files("www/layers"))
