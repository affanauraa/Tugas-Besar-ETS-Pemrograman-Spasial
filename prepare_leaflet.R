# =====================================================
# PERSIAPAN RASTER UNTUK WEBGIS LEAFLET
# KABUPATEN PURWOREJO
# =====================================================

library(terra)
library(sf)

# Mengatur lokasi project
setwd("C:/Users/user/Downloads/TUBES ETS PEMSPA")

# Membuat folder output Leaflet
dir.create(
  "output/leaflet",
  recursive = TRUE,
  showWarnings = FALSE
)

# Membuat folder sementara
dir.create(
  "output/temp_raster",
  recursive = TRUE,
  showWarnings = FALSE
)

# Pengaturan memori
terraOptions(
  memfrac = 0.2,
  tempdir = normalizePath("output/temp_raster"),
  progress = 1
)

# Definisi CRS
wkt_utm49s <- sf::st_crs(32749)$wkt
wkt_web <- sf::st_crs(3857)$wkt

# =====================================================
# FUNGSI MEMBUAT RASTER RINGAN
# =====================================================

buat_preview <- function(input, output, kategori = FALSE) {
  
  r <- rast(input)
  
  # Jika CRS belum lengkap tetapi namanya UTM 49S
  info_crs <- crs(r, describe = TRUE)
  
  if (is.na(info_crs$code[1]) &&
      grepl("UTM zone 49S", info_crs$name[1])) {
    crs(r) <- wkt_utm49s
  }
  
  if (!nzchar(crs(r))) {
    stop(paste("CRS raster kosong:", input))
  }
  
  # Batasi jumlah piksel
  target_sel <- 100000
  
  faktor <- max(
    1,
    ceiling(sqrt(ncell(r) / target_sel))
  )
  
  # Agregasi raster
  if (faktor > 1) {
    
    if (kategori) {
      r <- aggregate(
        r,
        fact = faktor,
        fun = "modal",
        na.rm = TRUE
      )
    } else {
      r <- aggregate(
        r,
        fact = faktor,
        fun = "mean",
        na.rm = TRUE
      )
    }
  }
  
  # Jika raster kategori berupa factor
  if (kategori && is.factor(r)) {
    r <- as.numeric(r)
  }
  
  # Reproject ke CRS Leaflet
  r <- project(
    r,
    wkt_web,
    method = if (kategori) "near" else "bilinear"
  )
  
  # Simpan hasil
  writeRaster(
    r,
    output,
    overwrite = TRUE,
    datatype = "FLT4S",
    gdal = "COMPRESS=LZW"
  )
  
  cat("SELESAI:", output, "\n")
}

# =====================================================
# MEMBUAT 5 RASTER LEAFLET
# =====================================================

buat_preview(
  "output/klasifikasi_crs_fixed.tif",
  "output/leaflet/klasifikasi_web.tif",
  kategori = TRUE
)

buat_preview(
  "output/NDVI_Purworejo.tif",
  "output/leaflet/ndvi_web.tif"
)

buat_preview(
  "output/NDBI_Purworejo.tif",
  "output/leaflet/ndbi_web.tif"
)

buat_preview(
  "output/NDWI_Purworejo.tif",
  "output/leaflet/ndwi_web.tif"
)

buat_preview(
  "output/NDCI_Purworejo.tif",
  "output/leaflet/ndci_web.tif"
)

# =====================================================
# CEK HASIL
# =====================================================

list.files("output/leaflet")