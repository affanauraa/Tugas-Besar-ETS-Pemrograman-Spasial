
# =====================================================
# BAGIAN 1 - MEMANGGIL PACKAGE
# =====================================================

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(ggspatial)


# =====================================================
# BAGIAN 2 - LOKASI PROJECT
# =====================================================

setwd("C:/Users/user/Downloads/TUBES ETS PEMSPA")

# Periksa lokasi dan file
getwd()
list.files()
list.files("data")


# =====================================================
# BAGIAN 3 - IMPORT BATAS ADMINISTRASI
# =====================================================

file_kab <- "data/FILE BATAS ADM/Batas Adm Kabupaten_Purworejo.shp"

file_kec <- "data/FILE BATAS ADM/Batas Adm Kecamatan_Purworejo.shp"

# Memastikan file tersedia
stopifnot(file.exists(file_kab))
stopifnot(file.exists(file_kec))

# Membaca shapefile
kabupaten <- st_read(file_kab)
kecamatan <- st_read(file_kec)

# Memeriksa CRS shapefile
st_crs(kabupaten)
st_crs(kecamatan)

# Memastikan CRS sumber tersedia
if (is.na(st_crs(kabupaten)) ||
    is.na(st_crs(kecamatan))) {
  stop("CRS shapefile belum diketahui. Periksa file .prj.")
}

# Memeriksa jumlah kecamatan
nrow(kecamatan)

# Menampilkan batas administrasi
plot(
  st_geometry(kabupaten),
  col = "lightyellow",
  border = "black",
  main = "Batas Administrasi Kabupaten Purworejo"
)

plot(
  st_geometry(kecamatan),
  add = TRUE,
  border = "red"
)


# =====================================================
# BAGIAN 4 - MEMBACA BAND SENTINEL-2
# =====================================================

# Mencari semua file JP2 di folder data
daftar_jp2 <- list.files(
  "data",
  pattern = "\\.jp2$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

# Melihat file JP2 yang ditemukan
basename(daftar_jp2)

# Fungsi membaca band
ambil_band <- function(kode, resolusi) {
  
  pola <- paste0(
    "_", kode, "_", resolusi, "m\\.jp2$"
  )
  
  file <- grep(
    pola,
    daftar_jp2,
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (length(file) != 1) {
    stop(paste(
      "Band", kode,
      "tidak ditemukan atau ditemukan lebih dari satu."
    ))
  }
  
  rast(file)
}

# Band resolusi 10 meter
B02 <- ambil_band("B02", 10)
B03 <- ambil_band("B03", 10)
B04 <- ambil_band("B04", 10)
B08 <- ambil_band("B08", 10)

# Band resolusi 20 meter
B05 <- ambil_band("B05", 20)
B11 <- ambil_band("B11", 20)
SCL <- ambil_band("SCL", 20)


# =====================================================
# BAGIAN 5 - MEMPERBAIKI CRS CITRA
# =====================================================

# CRS asli citra sudah diverifikasi melalui GDAL
# WGS 84 / UTM Zone 49S, EPSG:32749

# Mengambil definisi WKT resmi
wkt_utm49s <- sf::st_crs(32749)$wkt

# Menetapkan CRS pada seluruh band
terra::crs(B02) <- wkt_utm49s
terra::crs(B03) <- wkt_utm49s
terra::crs(B04) <- wkt_utm49s
terra::crs(B08) <- wkt_utm49s
terra::crs(B05) <- wkt_utm49s
terra::crs(B11) <- wkt_utm49s
terra::crs(SCL) <- wkt_utm49s

# Memeriksa hasil CRS
terra::crs(B04, describe = TRUE)
terra::crs(B11, describe = TRUE)
terra::crs(SCL, describe = TRUE)

# Memastikan CRS dikenali
stopifnot(
  !is.na(terra::crs(B04, describe = TRUE)$code[1])
)


# =====================================================
# BAGIAN 6 - MEMERIKSA RESOLUSI
# =====================================================

res(B02)
res(B03)
res(B04)
res(B08)

res(B05)
res(B11)
res(SCL)

# Hasil yang diharapkan:
# B02, B03, B04, B08 = 10 10
# B05, B11, SCL = 20 20


# =====================================================
# BAGIAN 7 - MENYAMAKAN RESOLUSI RASTER
# =====================================================

# B05 dari 20 meter menjadi grid 10 meter
B05_10 <- resample(
  B05,
  B04,
  method = "bilinear"
)

# B11 dari 20 meter menjadi grid 10 meter
B11_10 <- resample(
  B11,
  B04,
  method = "bilinear"
)

# SCL adalah data kategori
# Gunakan nearest neighbour
SCL_10 <- resample(
  SCL,
  B04,
  method = "near"
)

# Memeriksa kesamaan grid
compareGeom(
  B04,
  B05_10,
  B11_10,
  SCL_10
)


# =====================================================
# BAGIAN 8 - MENYAMAKAN CRS BATAS ADMINISTRASI
# =====================================================

# Mengubah shapefile menjadi SpatVector
kab_vect <- terra::vect(kabupaten)
kec_vect <- terra::vect(kecamatan)

# Memeriksa CRS sumber
terra::crs(kab_vect, describe = TRUE)
terra::crs(kec_vect, describe = TRUE)

# Mengubah CRS ke UTM Zone 49S
kab_vect <- terra::project(
  kab_vect,
  wkt_utm49s
)

kec_vect <- terra::project(
  kec_vect,
  wkt_utm49s
)

# Memeriksa CRS hasil transformasi
terra::crs(kab_vect, describe = TRUE)
terra::crs(kec_vect, describe = TRUE)


# =====================================================
# BAGIAN 9 - MENAMPILKAN CITRA DAN BATAS KABUPATEN
# =====================================================

# Menampilkan band merah Sentinel-2
plot(
  B04,
  main = "Citra Sentinel-2 dan Batas Kabupaten Purworejo"
)

# Menambahkan batas kabupaten
lines(
  kab_vect,
  col = "red",
  lwd = 2
)

# Menambahkan batas kecamatan
lines(
  kec_vect,
  col = "yellow",
  lwd = 1
)


# =====================================================
# BAGIAN 10 - PEMERIKSAAN AKHIR
# =====================================================

# Memeriksa CRS
terra::crs(B04, describe = TRUE)

# Memeriksa resolusi raster hasil resampling
res(B05_10)
res(B11_10)
res(SCL_10)

# Memeriksa kesamaan grid
compareGeom(
  B04,
  B05_10,
  B11_10,
  SCL_10
)

# Memeriksa lokasi raster dan kabupaten
ext(B04)
ext(kab_vect)

# Memeriksa jumlah kecamatan
nrow(kecamatan)


# =====================================================
# BAGIAN 11 - MENGGABUNGKAN BAND CITRA
# =====================================================

semua_band <- c(
  B02,
  B03,
  B04,
  B05_10,
  B08,
  B11_10,
  SCL_10
)

names(semua_band) <- c(
  "B02",
  "B03",
  "B04",
  "B05",
  "B08",
  "B11",
  "SCL"
)

semua_band

# Memeriksa apakah batas kabupaten berada
# dalam cakupan citra
ext(B04)
ext(kab_vect)


# =====================================================
# BAGIAN 12 - CROPPING DAN MASKING
# =====================================================

# Crop citra berdasarkan batas kabupaten
citra_crop <- crop(
  semua_band,
  kab_vect
)

# Mask area di luar Kabupaten Purworejo
citra_crop <- mask(
  citra_crop,
  kab_vect
)

# Menampilkan hasil
plot(
  citra_crop[["B04"]],
  main = "Hasil Crop Kabupaten Purworejo"
)

lines(
  kab_vect,
  col = "red",
  lwd = 2
)

# =====================================================
# BAGIAN 13 - MENYIMPAN HASIL CROP
# =====================================================

writeRaster(
  citra_crop,
  "output/citra_crop_purworejo.tif",
  overwrite = TRUE
)

# =====================================================
# BAGIAN 14 - VISUALISASI RGB
# =====================================================

rgb <- citra_crop[[
  c("B04", "B03", "B02")
]]

plotRGB(
  rgb,
  r = 1,
  g = 2,
  b = 3,
  stretch = "hist",
  main = "Citra RGB Kabupaten Purworejo"
)


# =====================================================
# BAGIAN 15 - MENGHITUNG INDEKS SPEKTRAL
# =====================================================

# MENYIAPKAN BAND HASIL CROP
b03 <- citra_crop[["B03"]]
b04 <- citra_crop[["B04"]]
b05 <- citra_crop[["B05"]]
b08 <- citra_crop[["B08"]]
b11 <- citra_crop[["B11"]]
scl <- citra_crop[["SCL"]]

# HANYA contoh jika metadata mengonfirmasi
# offset -1000 untuk seluruh band ini
# dan quantification value 10000

b03 <- (b03 - 1000) / 10000
b04 <- (b04 - 1000) / 10000
b05 <- (b05 - 1000) / 10000
b08 <- (b08 - 1000) / 10000
b11 <- (b11 - 1000) / 10000

# 1. MENGHITUNG NDVI
NDVI <- ifel(
  (b08 + b04) == 0,
  NA,
  (b08 - b04) / (b08 + b04)
)

names(NDVI) <- "NDVI"

plot(
  NDVI,
  main = "NDVI Kabupaten Purworejo"
)

# 2. MENGHITUNG NDBI
NDBI <- ifel(
  (b11 + b08) == 0,
  NA,
  (b11 - b08) / (b11 + b08)
)

names(NDBI) <- "NDBI"

plot(
  NDBI,
  main = "NDBI Kabupaten Purworejo"
)

# 3. MENGHITUNG NDWI
NDWI <- ifel(
  (b03 + b08) == 0,
  NA,
  (b03 - b08) / (b03 + b08)
)

names(NDWI) <- "NDWI"

plot(
  NDWI,
  main = "NDWI Kabupaten Purworejo"
)

# 4. MENGHITUNG NDCI
NDCI <- ifel(
  (b05 + b04) == 0,
  NA,
  (b05 - b04) / (b05 + b04)
)

names(NDCI) <- "NDCI"

plot(
  NDCI,
  main = "NDCI Kabupaten Purworejo"
)

# 5. Kategori SCL untuk awan
awan <- ifel(
  scl %in% c(8, 9, 10),
  1,
  0
)

names(awan) <- "Awan"

plot(
  awan,
  main = "Mask Awan Kabupaten Purworejo"
)

## MENYIMPAN HASIL IDENTIFIKASI INDEKS SPEKTRAL
# a. Simpan NDVI
writeRaster(
  NDVI,
  "output/NDVI_Purworejo.tif",
  overwrite = TRUE
)

# b. Simpan NDBI
writeRaster(
  NDBI,
  "output/NDBI_Purworejo.tif",
  overwrite = TRUE
)

# c. Simpan NDWI
writeRaster(
  NDWI,
  "output/NDWI_Purworejo.tif",
  overwrite = TRUE
)

# d. Simpan NDCI
writeRaster(
  NDCI,
  "output/NDCI_Purworejo.tif",
  overwrite = TRUE
)

# =====================================================
# BAGIAN 15 - KLASIFIKASI TUTUPAN LAHAN
# =====================================================

# Menggabungkan keempat indeks
indeks <- c(
  NDVI,
  NDBI,
  NDWI,
  NDCI
)

# Memberikan nama layer
names(indeks) <- c(
  "NDVI",
  "NDBI",
  "NDWI",
  "NDCI"
)

# Memeriksa hasil penggabungan
indeks

# Memeriksa jumlah layer
nlyr(indeks)

# Menampilkan keempat indeks
plot(indeks)

# Menyimpan indeks gabungan
writeRaster(
  indeks,
  "output/indeks_spektral_purworejo.tif",
  overwrite = TRUE
)

# Menentukan Treshold
hist(NDVI, main = "Histogram NDVI")
hist(NDBI, main = "Histogram NDBI")
hist(NDWI, main = "Histogram NDWI")
hist(NDCI, main = "Histogram NDCI")

# Klasifikasi Kategori

## Kelas 1 = Vegetasi
## Kelas 2 = Bangunan
## Kelas 3 = Perairan
## Kelas 4 = Awan

klasifikasi <- ifel(
  awan == 1, 4,
  ifel(
    NDWI > 0.15, 3,
    ifel(
      NDVI > 0.30, 1,
      ifel(
        NDBI > 0.05, 2,
        NA
      )
    )
  )
)

names(klasifikasi) <- "kelas"

# Menyimpan hasil
writeRaster(
  klasifikasi,
  "output/klasifikasi_purworejo.tif",
  overwrite = TRUE
)

# Memberikan warna tiap kategori
# Membuat klasifikasi sebagai data kategori
klasifikasi <- as.factor(klasifikasi)

levels(klasifikasi) <- data.frame(
  value = 1:4,
  kelas = c(
    "Vegetasi",
    "Bangunan",
    "Perairan",
    "Awan"
  )
)

# Warna
warna_kelas <- c(
  "Vegetasi" = "#329447",
  "Bangunan" = "#D96050",
  "Perairan" = "#3589CD",
  "Awan" = "#D9D9D9"
)

plot(
  klasifikasi,
  col = unname(warna_kelas),
  main = "Klasifikasi Tutupan Lahan Purworejo"
)


# =====================================================
# BAGIAN 16 - VISUALISASI PETA
# =====================================================

# Mengubah batas administrasi ke CRS raster
kab_peta <- st_transform(
  kabupaten,
  crs = st_crs(crs(klasifikasi))
)

kec_peta <- st_transform(
  kecamatan,
  crs = st_crs(crs(klasifikasi))
)

# Membuat peta
peta_final <- ggplot() +
  
  geom_spatraster(
    data = klasifikasi
  ) +
  
  scale_fill_manual(
    values = warna_kelas,
    na.value = "transparent",
    drop = FALSE,
    name = "Tutupan Lahan"
  ) +
  
  geom_sf(
    data = kec_peta,
    fill = NA,
    color = "grey40",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = kab_peta,
    fill = NA,
    color = "black",
    linewidth = 0.7
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  
  labs(
    title = "Peta Klasifikasi Tutupan Lahan",
    subtitle = "Kabupaten Purworejo, Jawa Tengah",
    caption = paste(
      "Sumber: Sentinel-2 Level-2A,",
      "26 Juli 2026; Ina-Geoportal.",
      "Pengolahan: RStudio."
    )
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.position = "right"
  )

# Menampilkan hasil
print(peta_final)

ggsave(
  filename = "output/Peta_Klasifikasi_Purworejo.pdf",
  plot = peta_final,
  width = 11.7,
  height = 8.3,
  units = "in"
)


# =====================================================
# BAGIAN 17 - ZONAL STATISTICS PER KECAMATAN
# =====================================================

# Membuat indeks tanpa piksel awan
indeks_bersih <- ifel(
  awan == 1,
  NA,
  indeks,
  filename = "output/indeks_bersih_purworejo.tif",
  overwrite = TRUE
)

# Memberikan nama layer
names(indeks_bersih) <- c(
  "NDVI",
  "NDBI",
  "NDWI",
  "NDCI"
)

# Memastikan terdapat 4 layer
nlyr(indeks_bersih)

# Memastikan batas kecamatan sesuai CRS
kec_vect <- terra::project(
  terra::vect(kecamatan),
  terra::crs(indeks_bersih)
)

# Memeriksa hasil
terra::crs(kec_vect, describe = TRUE)

## MENGHITUNG STATISTIK TIAP KECAMATAN

# 1. Mean
hasil_mean <- terra::extract(
  indeks_bersih,
  kec_vect,
  fun = mean,
  na.rm = TRUE
)

# 2. Minimum
hasil_min <- terra::extract(
  indeks_bersih,
  kec_vect,
  fun = min,
  na.rm = TRUE
)

# 3. Maximum
hasil_max <- terra::extract(
  indeks_bersih,
  kec_vect,
  fun = max,
  na.rm = TRUE
)

# 4. Median
hasil_median <- terra::extract(
  indeks_bersih,
  kec_vect,
  fun = median,
  na.rm = TRUE
)

# 5. Standar deviasi
hasil_sd <- terra::extract(
  indeks_bersih,
  kec_vect,
  fun = sd,
  na.rm = TRUE
)

## MENGGABUNGKAN SELURUH HASIL NILAI STATISTIK

# Menambahkan akhiran nama statistik
rapikan <- function(data, akhiran) {
  
  names(data)[-1] <- paste0(
    names(data)[-1],
    "_",
    akhiran
  )
  
  return(data)
}

hasil_mean <- rapikan(hasil_mean, "mean")
hasil_min <- rapikan(hasil_min, "min")
hasil_max <- rapikan(hasil_max, "max")
hasil_median <- rapikan(hasil_median, "median")
hasil_sd <- rapikan(hasil_sd, "sd")

# Gabungkan statistik berdasarkan ID
tabel_zonal <- hasil_mean |>
  left_join(hasil_min, by = "ID") |>
  left_join(hasil_max, by = "ID") |>
  left_join(hasil_median, by = "ID") |>
  left_join(hasil_sd, by = "ID")

# Tambahkan atribut kecamatan
atribut_kecamatan <- st_drop_geometry(kecamatan)

tabel_zonal <- bind_cols(
  atribut_kecamatan,
  tabel_zonal
)

# Menampilkan tabel
View(tabel_zonal)

# Simpan hasil tabel statistik
write.csv(
  tabel_zonal,
  "output/Zonal_Statistics_Purworejo.csv",
  row.names = FALSE
)