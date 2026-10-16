
# =====================================================
# WEBGIS KABUPATEN PURWOREJO
# R SHINY + LEAFLET
# MENGGUNAKAN PNG HASIL ANALISIS SENTINEL-2
# =====================================================

# 1. PACKAGE
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(DT)
library(htmltools)
library(htmlwidgets)
library(dplyr)

# 2. FOLDER PROJECT
setwd("C:/Users/user/Downloads/TUBES ETS PEMSPA")

# 3. BATAS ADMINISTRASI
kab <- sf::st_read(
  "data/FILE BATAS ADM/Batas Adm Kabupaten_Purworejo.shp",
  quiet = TRUE
)

kec <- sf::st_read(
  "data/FILE BATAS ADM/Batas Adm Kecamatan_Purworejo.shp",
  quiet = TRUE
)


# =====================================================
# MEMPERBAIKI GEOMETRI BATAS KABUPATEN
# =====================================================

# Pastikan CRS kabupaten benar
if (is.na(sf::st_crs(kab)$epsg)) {
  sf::st_crs(kab) <- sf::st_crs(32749)
}

# Memperbaiki geometri kabupaten
kab <- sf::st_make_valid(kab)

# Mengubah koordinat ke EPSG:4326
kab <- sf::st_transform(kab, 4326)

# Menyederhanakan geometri untuk Leaflet
kab <- sf::st_simplify(
  kab,
  dTolerance = 0.0001,
  preserveTopology = TRUE
)

# Mengambil geometri polygon
kab <- sf::st_collection_extract(
  kab,
  "POLYGON",
  warn = FALSE
)

# Menghilangkan dimensi tambahan
kab <- sf::st_zm(
  kab,
  drop = TRUE,
  what = "ZM"
)

# Batas kecamatan tetap seperti sebelumnya
kec <- sf::st_transform(kec, 4326)

# =====================================================
# NAMA KECAMATAN
# =====================================================

nama_kec <- as.character(kec$Nama.wilay)

nama_kec[is.na(nama_kec) | nama_kec == ""] <-
  "Nama tidak tersedia"

kec$nama_kec <- nama_kec

# 4. BATAS GEOGRAFIS GAMBAR
batas_layer <- readRDS(
  "www/layers/batas_layer.rds"
)


# =====================================================
# 5. ZONAL STATISTICS DAN PENGGABUNGAN DATA
# =====================================================

zonal <- read.csv(
  "output/Zonal_Statistics_Purworejo.csv",
  check.names = FALSE
)

# Membuat kunci kecamatan dari atribut sebenarnya
kec$kode_join <- trimws(
  tolower(as.character(kec$Nama.wilay))
)

zonal$kode_join <- trimws(
  tolower(as.character(zonal$Nama.wilay))
)

# Memeriksa apakah nama kecamatan unik
if (anyDuplicated(kec$kode_join) > 0 ||
    anyDuplicated(zonal$kode_join) > 0) {
  stop(
    "Nama kecamatan tidak unik. Periksa data sebelum join."
  )
}

# Menentukan kolom statistik yang akan digunakan
kolom_stat <- c(
  "NDVI_mean",
  "NDBI_mean",
  "NDWI_mean",
  "NDCI_mean"
)

# Memastikan keempat kolom tersedia
stopifnot(all(kolom_stat %in% names(zonal)))

# Mencocokkan data berdasarkan nama kecamatan
indeks_join <- match(
  kec$kode_join,
  zonal$kode_join
)

# Pastikan tidak ada kecamatan yang gagal dicocokkan
if (anyNA(indeks_join)) {
  stop(
    paste(
      "Kecamatan tidak ditemukan di CSV:",
      paste(
        kec$Nama.wilay[is.na(indeks_join)],
        collapse = ", "
      )
    )
  )
}

# Memasukkan nilai statistik ke layer kecamatan
for (kolom in kolom_stat) {
  kec[[kolom]] <- zonal[[kolom]][indeks_join]
}

# Memastikan jumlah kecamatan tetap 16
stopifnot(nrow(kec) == 16)

# Format angka untuk popup
format_nilai <- function(x) {
  ifelse(
    is.na(x),
    "Tidak tersedia",
    sprintf("%.3f", as.numeric(x))
  )
}

# 6. PALET LEGENDA
warna_klas <- c(
  "#329447",
  "#D96050",
  "#3589CD",
  "#D9D9D9"
)

pal_ndvi <- colorNumeric("RdYlGn", c(-1, 1))
pal_ndbi <- colorNumeric("YlOrRd", c(-1, 1))
pal_ndwi <- colorNumeric("Blues", c(-1, 1))
pal_ndci <- colorNumeric("viridis", c(-1, 1))

bbox <- sf::st_bbox(kab)

# =====================================================
# 7. UI DASHBOARD
# =====================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "WebGIS Purworejo",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    
    sidebarMenu(
      menuItem(
        "Peta Interaktif",
        tabName = "peta",
        icon = icon("map")
      ),
      
      menuItem(
        "Zonal Statistics",
        tabName = "statistik",
        icon = icon("table")
      )
    ),
    
    br(),
    
    selectInput(
      "pilih_layer",
      "Pilih Layer Peta",
      choices = c(
        "Klasifikasi Tutupan Lahan" = "klasifikasi",
        "NDVI" = "ndvi",
        "NDBI" = "ndbi",
        "NDWI" = "ndwi",
        "NDCI" = "ndci"
      ),
      selected = "klasifikasi"
    ),
    
    checkboxInput(
      "tampil_kab",
      "Batas Kabupaten",
      TRUE
    ),
    
    checkboxInput(
      "tampil_kec",
      "Batas Kecamatan",
      TRUE
    ),
    
    sliderInput(
      "opacity",
      "Opasitas Peta",
      min = 0.2,
      max = 1,
      value = 0.85,
      step = 0.05
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # HALAMAN PETA
      tabItem(
        tabName = "peta",
        
        box(
          width = 12,
          title = "Peta Analisis Sentinel-2 Purworejo",
          status = "primary",
          solidHeader = TRUE,
          
          leafletOutput(
            "peta_leaflet",
            height = 650
          )
        ),
        
        box(
          width = 12,
          title = "Informasi Peta",
          
          p("Wilayah: Kabupaten Purworejo, Jawa Tengah."),
          
          p("Sumber: Sentinel-2 Level-2A, 26 Juli 2026."),
          
          p(
            "Layer: Klasifikasi Tutupan Lahan,",
            "NDVI, NDBI, NDWI, dan NDCI."
          )
        )
      ),
      
      # HALAMAN STATISTIK
      tabItem(
        tabName = "statistik",
        
        box(
          width = 12,
          title = "Zonal Statistics per Kecamatan",
          status = "primary",
          solidHeader = TRUE,
          
          downloadButton(
            "download_csv",
            "Download CSV"
          ),
          
          br(),
          br(),
          
          DTOutput("tabel_zonal")
        )
      )
    )
  )
)

# =====================================================
# 8. SERVER
# =====================================================

server <- function(input, output, session) {
  
  # Menentukan informasi layer aktif
  layer_aktif <- reactive({
    
    nama <- input$pilih_layer
    
    list(
      nama = nama,
      
      url = paste0(
        "layers/",
        nama,
        ".png"
      ),
      
      bounds = batas_layer[[nama]],
      
      opacity = input$opacity
    )
  })
  
  # ===================================================
  # 9. PETA LEAFLET
  # ===================================================
  
  output$peta_leaflet <- renderLeaflet({
    
    req(input$pilih_layer)
    
    info <- layer_aktif()
    
    # Membuat peta dasar
    peta <- leaflet() |>
      
      addProviderTiles(
        providers$OpenStreetMap,
        group = "OpenStreetMap"
      ) |>
      
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Citra Satelit"
      ) |>
      
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Citra Satelit"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) |>
      
      fitBounds(
        bbox[["xmin"]],
        bbox[["ymin"]],
        bbox[["xmax"]],
        bbox[["ymax"]]
      )
    
    # 10. LEGENDA
    if (input$pilih_layer == "klasifikasi") {
      
      peta <- peta |>
        addLegend(
          position = "bottomright",
          colors = warna_klas,
          labels = c(
            "Vegetasi",
            "Bangunan",
            "Perairan",
            "Awan"
          ),
          title = "Tutupan Lahan"
        )
      
    } else {
      
      pal <- switch(
        input$pilih_layer,
        ndvi = pal_ndvi,
        ndbi = pal_ndbi,
        ndwi = pal_ndwi,
        ndci = pal_ndci
      )
      
      peta <- peta |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = seq(-1, 1, length.out = 100),
          title = toupper(input$pilih_layer)
        )
    }
    
    
    # =====================================================
    # 11. BATAS KECAMATAN DENGAN POPUP STATISTIK
    # =====================================================
    
    if (isTRUE(input$tampil_kec)) {
      
      popup_kec <- paste0(
        "<div style='min-width:230px; font-family:Arial;'>",
        
        "<div style='font-size:16px; font-weight:bold;",
        "margin-bottom:8px;'>",
        htmltools::htmlEscape(kec$nama_kec),
        "</div>",
        
        "<div style='font-size:12px; color:#777;",
        "margin-bottom:10px;'>",
        "Kabupaten Purworejo, Jawa Tengah",
        "</div>",
        
        "<hr style='margin:5px 0 10px;'>",
        
        "<b>Rata-rata NDVI:</b> ",
        format_nilai(kec$NDVI_mean),
        "<br>",
        
        "<b>Rata-rata NDBI:</b> ",
        format_nilai(kec$NDBI_mean),
        "<br>",
        
        "<b>Rata-rata NDWI:</b> ",
        format_nilai(kec$NDWI_mean),
        "<br>",
        
        "<b>Rata-rata NDCI:</b> ",
        format_nilai(kec$NDCI_mean),
        
        "</div>"
      )
      
      peta <- peta |>
        addPolygons(
          data = kec,
          color = "#F1D54B",
          weight = 1.2,
          fill = TRUE,
          fillColor = "#F1D54B",
          fillOpacity = 0.01,
          popup = popup_kec,
          highlightOptions = highlightOptions(
            color = "#FFFFFF",
            weight = 3,
            bringToFront = TRUE
          )
        )
    }
    
    
    # =================================================
    # 13. OVERLAY PNG DENGAN LEAFLET JAVASCRIPT
    # =================================================
    
    # Menggunakan L.imageOverlay() melalui htmlwidgets
    # sehingga tidak perlu membaca CRS GeoTIFF.
    
    peta <- htmlwidgets::onRender(
      
      peta,
      
      "
      function(el, x, data) {

        var map = this;

        var image = L.imageOverlay(
          data.url,
          data.bounds,
          {
            opacity: data.opacity,
            interactive: false
          }
        );

        image.addTo(map);

        // Menempatkan gambar di belakang garis batas
        image.bringToBack();
      }
      ",
      
      data = list(
        url = info$url,
        bounds = info$bounds,
        opacity = info$opacity
      )
    )
    
    peta
  })
  
  # ===================================================
  # 14. TABEL ZONAL STATISTICS
  # ===================================================
  
  output$tabel_zonal <- DT::renderDT({
    
    DT::datatable(
      zonal,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 16,
        scrollX = TRUE
      )
    )
  })
  
  # 15. DOWNLOAD CSV
  output$download_csv <- downloadHandler(
    
    filename = function() {
      "Zonal_Statistics_Purworejo.csv"
    },
    
    content = function(file) {
      
      write.csv(
        zonal,
        file,
        row.names = FALSE
      )
    }
  )
}

# =====================================================
# 16. MENJALANKAN WEBGIS
# =====================================================

shinyApp(ui, server)
