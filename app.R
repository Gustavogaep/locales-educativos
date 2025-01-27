# --------------------------------------------------------------------------------
# 1) Carga de librerías
# --------------------------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(rmapshaper)
library(tidyr)

# --------------------------------------------------------------------------------
# 2) Rutas a los archivos y lectura
# --------------------------------------------------------------------------------
data_path <- "data"  # Carpeta donde están los datos

shapefile_departamentos <- file.path(data_path, "Departamentos.geojson")
shapefile_provincias    <- file.path(data_path, "Provincias.geojson")
shapefile_distritos     <- file.path(data_path, "Distritos.geojson")
data_csv                <- file.path(data_path, "paraShiny4.csv")

# --------------------------------------------------------------------------------
# 3) Carga de datos espaciales y simplificación
# --------------------------------------------------------------------------------
departamentos <- st_read(shapefile_departamentos, quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  ms_simplify(keep = 1, keep_shapes = TRUE) %>%
  mutate(FIRST_IDDP = as.character(as.numeric(FIRST_IDDP)))

provincias <- st_read(shapefile_provincias, quiet = TRUE) %>%
  st_transform(crs = 4326) %>%
  ms_simplify(keep = 1, keep_shapes = TRUE) %>%
  mutate(FIRST_IDPR = as.character(as.numeric(FIRST_IDPR)))

distritos <- st_read(shapefile_distritos, quiet = TRUE) %>%
  filter(!st_is_empty(geometry)) %>%
  st_transform(crs = 4326) %>%
  ms_simplify(keep = 1, keep_shapes = TRUE) %>%
  mutate(IDDIST = as.character(as.numeric(IDDIST)))

# --------------------------------------------------------------------------------
# 4) Carga de datos tabulares
# --------------------------------------------------------------------------------
data <- read.csv(data_csv, stringsAsFactors = FALSE, encoding = "UTF-8")

# --------------------------------------------------------------------------------
# 5) Procesamiento de datos
# --------------------------------------------------------------------------------
data <- data %>%
  mutate(
    FIRST_IDDP   = as.character(as.numeric(FIRST_IDDP)),
    FIRST_IDPR   = as.character(as.numeric(FIRST_IDPR)),
    IDDIST       = as.character(as.numeric(IDDIST)),
    
    DPTO         = as.character(DPTO),
    PROV         = as.character(PROV),
    DIST         = as.character(DIST),
    FUENTE_AGUA  = trimws(FUENTE_AGUA),
    TIPO_DESAGUE = trimws(TIPO_DESAGUE),
    FUENTE_ELEC  = trimws(FUENTE_ELEC),
    servbasic    = trimws(servbasic),
    tipo_gest    = as.character(tipo_gest),
    AREA_CENSO2  = as.character(AREA_CENSO2)
  )

# Mapeos (sin tildes)
AGUA_MAPPING <- c(
  "Red publica"      = "Agua_Red_publica",
  "Camion cisterna"  = "Agua_Camion_cisterna",
  "Captacion lluvia" = "Agua_Captacion_lluvia",
  "No tiene"         = "Agua_No_tiene",
  "Otro"             = "Agua_Otro",
  "Pilon publico"    = "Agua_Pilon_publico",
  "Pozo"             = "Agua_de_pozo",
  "Rio o acequia"    = "Agua_de_rio_acequia"
)

DESAGUE_MAPPING <- c(
  "Red publica"            = "Desague_Red_publica",
  "Rio o acequia o canal"  = "Desague_Rio_acequia_o_canal",
  "Tanque septico"         = "Desague_tanque_septico",
  "Biodigestor"            = "Desague_Biodigestor",
  "Pozo sin tratamiento"   = "Desague_Pozo_sin_tratamiento",
  "UBSC"                   = "Desague_ubsc",
  "Otro"                   = "Desague_Otro",
  "No tiene"               = "Desague_No_tiene"
)

ELECTRICIDAD_MAPPING <- c(
  "Red publica"               = "Electricidad_Red_publica",
  "Motor de municipio"        = "Electricidad_motor_de_municipio",
  "Motor de la comunidad"     = "Electricidad_motor_de_comunidad",
  "Motor del local educativo" = "Electricidad_motor_de_local_educativo",
  "Panel solar"               = "Electricidad_Panel_solar",
  "Energia eolica"            = "Electricidad_eolica",
  "Otro"                      = "Electricidad_Otro",
  "No tiene"                  = "Electricidad_No_tiene"
)

VAR_MAPPING <- list(
  "FUENTE_AGUA"   = AGUA_MAPPING,
  "TIPO_DESAGUE"  = DESAGUE_MAPPING,
  "FUENTE_ELEC"   = ELECTRICIDAD_MAPPING
)

# Crear columnas binarias 0/1 en 'data'
for (variable in names(VAR_MAPPING)) {
  mapping <- VAR_MAPPING[[variable]]
  for (cat_label in names(mapping)) {
    col_name <- mapping[cat_label]
    data[[col_name]] <- ifelse(data[[variable]] == cat_label, 1, 0)
  }
}

# --------------------------------------------------------------------------------
# 6) Interfaz de usuario
# --------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Locales 2023",
    # Aquí añadimos el botón a la derecha:
    tags$li(
      a(
        href = "https://docs.google.com/spreadsheets/d/1yXZcW4soKpdwUuA-uCNaYRG7P6_lOU3Z/edit?usp=sharing&ouid=100865139892644416054&rtpof=true&sd=true",
        target = "_blank",
        class = "btn btn-success",
        style = "margin: 4px 6px; padding: 4px 8px; color: #FFFFFF;", 
        "Obtener dataset"
      ),
      class = "dropdown"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa de Calor", tabName = "heatmap", icon = icon("globe")),
      
      radioButtons(
        inputId  = "nivel",
        label    = "Seleccionar nivel geográfico:",
        choices  = c("Departamento","Provincia","Distrito"),
        selected = "Departamento",
        inline   = TRUE
      ),
      
      selectInput(
        inputId = "variable", 
        label   = "Seleccionar variable de interés:",
        choices = c("FUENTE_AGUA","TIPO_DESAGUE","FUENTE_ELEC","servbasic"),
        selected= "FUENTE_AGUA"
      ),
      
      uiOutput("filtros_variables"),
      
      selectInput("tipo_gest", "Tipo de Gestión:",
                  choices  = c("Seleccionar todas","Publica","Privada"),
                  selected = "Seleccionar todas"),
      
      selectInput("area_censo2", "Tipo de Área:",
                  choices  = c("Seleccionar todas","Urbano","Rural"),
                  selected = "Seleccionar todas"),
      
      pickerInput(
        inputId = "sel_depto",
        label   = "Departamento:",
        choices = NULL,
        selected= NULL,
        multiple= FALSE,
        options = list(`live-search` = TRUE)
      ),
      
      pickerInput(
        inputId = "sel_prov",
        label   = "Provincia:",
        choices = NULL,
        selected= NULL,
        multiple= FALSE,
        options = list(`live-search` = TRUE)
      ),
      
      pickerInput(
        inputId = "sel_dist",
        label   = "Distrito:",
        choices = NULL,
        selected= NULL,
        multiple= FALSE,
        options = list(`live-search` = TRUE)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName="heatmap",
        fluidRow(
          # Columna izquierda con box y mapa
          column(
            width = 6,
            box(
              title = "Mapa de Calor", width = 12, status = "primary", solidHeader = TRUE,
              leafletOutput("mapa", height = "550px")
            )
          ),
          # Columna derecha con box y gráfico de barras
          column(
            width = 6,
            box(
              title = "Gráfico de Barras", width = 12, status = "primary", solidHeader = TRUE,
              plotOutput("grafico_barras", height = "550px")
            )
          )
        )
      )
    )
  )
)

# --------------------------------------------------------------------------------
# 7) Lógica del servidor
# --------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # A) Listas de todos los deptos, provs, dists
  all_dptos <- sort(unique(data$DPTO))
  all_provs <- sort(unique(data$PROV))
  all_dists <- sort(unique(data$DIST))
  lbl_all   <- "Seleccionar todos"
  
  prepend_all <- function(x) c(lbl_all, x)
  
  # Inicializa combos
  updatePickerInput(session, "sel_depto",
                    choices = prepend_all(all_dptos),
                    selected= lbl_all)
  updatePickerInput(session, "sel_prov",
                    choices = prepend_all(all_provs),
                    selected= lbl_all)
  updatePickerInput(session, "sel_dist",
                    choices = prepend_all(all_dists),
                    selected= lbl_all)
  
  # Encadenar Deptos -> Provincias -> Distritos
  observeEvent(input$sel_depto, {
    req(input$sel_depto)
    
    if (input$sel_depto == lbl_all) {
      new_provs <- all_provs
    } else {
      new_provs <- data %>%
        filter(DPTO == input$sel_depto) %>%
        pull(PROV) %>%
        unique() %>%
        sort()
    }
    
    updatePickerInput(session, "sel_prov",
                      choices = prepend_all(new_provs),
                      selected= lbl_all)
    
    updatePickerInput(session, "sel_dist",
                      choices = prepend_all(all_dists),
                      selected= lbl_all)
  })
  
  observeEvent(input$sel_prov, {
    req(input$sel_prov)
    
    if (input$sel_prov == lbl_all) {
      new_dists <- if (input$sel_depto == lbl_all) {
        all_dists
      } else {
        data %>%
          filter(DPTO == input$sel_depto) %>%
          pull(DIST) %>%
          unique() %>%
          sort()
      }
    } else {
      if (input$sel_depto == lbl_all) {
        new_dists <- data %>%
          filter(PROV == input$sel_prov) %>%
          pull(DIST) %>%
          unique() %>%
          sort()
      } else {
        new_dists <- data %>%
          filter(DPTO == input$sel_depto, PROV == input$sel_prov) %>%
          pull(DIST) %>%
          unique() %>%
          sort()
      }
    }
    
    updatePickerInput(session, "sel_dist",
                      choices = prepend_all(new_dists),
                      selected= lbl_all)
  })
  
  # B) UI dinámico para la variable seleccionada
  output$filtros_variables <- renderUI({
    req(input$variable)
    var_elegida <- input$variable
    
    valores_unicos <- sort(unique(data[[var_elegida]]))
    
    selectInput(
      inputId  = "filtro_variable",
      label    = paste("Seleccionar", var_elegida, ":"),
      choices  = valores_unicos,
      multiple = FALSE
    )
  })
  
  # C) Forzar valor por defecto tras cambiar la variable
  observeEvent(input$variable, {
    var_elegida <- input$variable
    valores_unicos <- sort(unique(data[[var_elegida]]))
    
    if (length(valores_unicos) > 0) {
      # Definir reglas para default según la variable elegida
      if (var_elegida == "FUENTE_AGUA" && "Red publica" %in% valores_unicos) {
        updateSelectInput(session, "filtro_variable",
                          choices  = valores_unicos,
                          selected = "Red publica")
      } else if (var_elegida == "TIPO_DESAGUE" && "Red publica" %in% valores_unicos) {
        updateSelectInput(session, "filtro_variable",
                          choices  = valores_unicos,
                          selected = "Red publica")
      } else if (var_elegida == "FUENTE_ELEC" && "Red publica" %in% valores_unicos) {
        updateSelectInput(session, "filtro_variable",
                          choices  = valores_unicos,
                          selected = "Red publica")
      } else if (var_elegida == "servbasic" && "3 serv basicos" %in% valores_unicos) {
        updateSelectInput(session, "filtro_variable",
                          choices  = valores_unicos,
                          selected = "3 serv basicos")
      } else {
        # Si no hay regla específica o el valor por defecto no existe, tomamos el primero
        if (is.null(input$filtro_variable) || !(input$filtro_variable %in% valores_unicos)) {
          updateSelectInput(session, "filtro_variable",
                            choices  = valores_unicos,
                            selected = valores_unicos[1])
        } else {
          updateSelectInput(session, "filtro_variable",
                            choices  = valores_unicos,
                            selected = input$filtro_variable)
        }
      }
    } else {
      updateSelectInput(session, "filtro_variable",
                        choices  = character(0),
                        selected = character(0))
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # --------------------------------------------------------------------------------
  # Funciones reactivas para calcular agregados
  # --------------------------------------------------------------------------------
  datosMapa <- reactive({
    req(input$nivel, input$variable, input$filtro_variable)
    
    data_f <- data
    if (input$tipo_gest != "Seleccionar todas") {
      data_f <- data_f %>% filter(tipo_gest == input$tipo_gest)
    }
    if (input$area_censo2 != "Seleccionar todas") {
      data_f <- data_f %>% filter(AREA_CENSO2 == input$area_censo2)
    }
    
    if (input$nivel == "Departamento") {
      join_col_df <- "FIRST_IDDP"
      name_col    <- "DPTO"
    } else if (input$nivel == "Provincia") {
      join_col_df <- "FIRST_IDPR"
      name_col    <- "PROV"
    } else {
      join_col_df <- "IDDIST"
      name_col    <- "DIST"
    }
    
    df_total <- data_f %>%
      group_by(.data[[join_col_df]]) %>%
      summarise(total_locales = n(), .groups = "drop")
    
    var_actual <- input$variable
    cat_selecc <- input$filtro_variable
    
    if (var_actual %in% names(VAR_MAPPING)) {
      mapping      <- VAR_MAPPING[[var_actual]]
      selected_col <- mapping[cat_selecc]
      
      if (is.na(selected_col)) {
        df_variable <- data_f %>%
          group_by(.data[[join_col_df]]) %>%
          summarise(n_cumple = 0, .groups = "drop")
      } else {
        df_variable <- data_f %>%
          group_by(.data[[join_col_df]]) %>%
          summarise(n_cumple = sum(.data[[selected_col]], na.rm = TRUE),
                    .groups = "drop")
      }
    } else {
      df_variable <- data_f %>%
        filter(.data[[var_actual]] == cat_selecc) %>%
        group_by(.data[[join_col_df]]) %>%
        summarise(n_cumple = n(), .groups = "drop")
    }
    
    df_prop <- df_total %>%
      left_join(df_variable, by = join_col_df) %>%
      mutate(
        n_cumple   = replace_na(n_cumple, 0),
        proporcion = n_cumple / total_locales
      )
    
    df_names <- data_f %>%
      select(all_of(c(join_col_df, name_col))) %>%
      distinct() %>%
      group_by(.data[[join_col_df]]) %>%
      summarise(name_territorio = first(.data[[name_col]]), .groups = "drop")
    
    df_final <- df_prop %>%
      left_join(df_names, by = join_col_df)
    
    df_final
  })
  
  datosDeptoBarras <- reactive({
    req(input$variable, input$filtro_variable)
    
    data_f <- data
    if (input$tipo_gest != "Seleccionar todas") {
      data_f <- data_f %>% filter(tipo_gest == input$tipo_gest)
    }
    if (input$area_censo2 != "Seleccionar todas") {
      data_f <- data_f %>% filter(AREA_CENSO2 == input$area_censo2)
    }
    
    join_col_df <- "FIRST_IDDP"
    name_col    <- "DPTO"
    
    df_total <- data_f %>%
      group_by(.data[[join_col_df]]) %>%
      summarise(total_locales = n(), .groups = "drop")
    
    var_actual <- input$variable
    cat_selecc <- input$filtro_variable
    
    if (var_actual %in% names(VAR_MAPPING)) {
      mapping      <- VAR_MAPPING[[var_actual]]
      selected_col <- mapping[cat_selecc]
      
      if (is.na(selected_col)) {
        df_variable <- data_f %>%
          group_by(.data[[join_col_df]]) %>%
          summarise(n_cumple = 0, .groups = "drop")
      } else {
        df_variable <- data_f %>%
          group_by(.data[[join_col_df]]) %>%
          summarise(n_cumple = sum(.data[[selected_col]], na.rm = TRUE),
                    .groups = "drop")
      }
    } else {
      df_variable <- data_f %>%
        filter(.data[[var_actual]] == cat_selecc) %>%
        group_by(.data[[join_col_df]]) %>%
        summarise(n_cumple = n(), .groups = "drop")
    }
    
    df_prop <- df_total %>%
      left_join(df_variable, by = join_col_df) %>%
      mutate(
        n_cumple   = replace_na(n_cumple, 0),
        proporcion = n_cumple / total_locales
      )
    
    df_names <- data_f %>%
      select(all_of(c(join_col_df, name_col))) %>%
      distinct() %>%
      group_by(.data[[join_col_df]]) %>%
      summarise(name_territorio = first(.data[[name_col]]), .groups = "drop")
    
    df_final <- df_prop %>%
      left_join(df_names, by = join_col_df)
    
    df_final
  })
  
  # --------------------------------------------------------------------------------
  # Render del mapa Leaflet
  # --------------------------------------------------------------------------------
  output$mapa <- renderLeaflet({
    req(datosMapa())
    df_prop <- datosMapa()
    
    if (input$nivel == "Departamento") {
      selected_geo <- departamentos
      join_col_sf  <- "FIRST_IDDP"
      join_col_df  <- "FIRST_IDDP"
    } else if (input$nivel == "Provincia") {
      selected_geo <- provincias
      join_col_sf  <- "FIRST_IDPR"
      join_col_df  <- "FIRST_IDPR"
    } else {
      selected_geo <- distritos
      join_col_sf  <- "IDDIST"
      join_col_df  <- "IDDIST"
    }
    
    selected_geo <- selected_geo %>%
      left_join(df_prop, by = setNames(join_col_df, join_col_sf))
    
    if (!"proporcion" %in% colnames(selected_geo)) {
      showNotification("No se pudo calcular la proporción para el mapa.", type = "error")
      return(leaflet() %>% addTiles())
    }
    
    vals_proporcion <- selected_geo$proporcion
    vals_validos    <- vals_proporcion[is.finite(vals_proporcion)]
    
    if (length(vals_validos) == 0) {
      showNotification("No hay valores numéricos para la proporción en la selección actual.", type = "warning")
      return(leaflet() %>% addTiles())
    }
    
    rng <- range(vals_validos, na.rm = TRUE)
    if (rng[1] == rng[2]) {
      rng <- c(rng[1] - 0.01, rng[2] + 0.01)
    }
    
    pal <- colorNumeric("YlOrRd", domain = rng, na.color = "transparent")
    
    popup_text <- paste0(
      "<strong>Territorio:</strong> ",
      ifelse(is.na(selected_geo$name_territorio), "", selected_geo$name_territorio),
      "<br><strong>Total locales:</strong> ",
      ifelse(is.na(selected_geo$total_locales), 0, selected_geo$total_locales),
      "<br><strong>Cumplen variable:</strong> ",
      ifelse(is.na(selected_geo$n_cumple), 0, selected_geo$n_cumple),
      "<br><strong>Proporción:</strong> ",
      ifelse(is.na(selected_geo$proporcion), 0, round(selected_geo$proporcion * 100, 2)), "%"
    )
    
    leaflet(selected_geo) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(proporcion),
        fillOpacity = 0.7,
        color       = "black",
        weight      = 0.5,
        popup       = popup_text
      ) %>%
      addLegend(
        pal      = pal,
        values   = vals_validos,
        title    = "Proporción",
        position = "bottomright"
      )
  })
  
  # --------------------------------------------------------------------------------
  # Render del gráfico de barras (horizontal)
  # --------------------------------------------------------------------------------
  output$grafico_barras <- renderPlot({
    req(datosDeptoBarras())
    
    df_res <- datosDeptoBarras()
    df_res <- df_res %>% filter(is.finite(proporcion))
    
    # Ordenar de mayor a menor
    df_res <- df_res %>% arrange(desc(proporcion))
    
    ggplot(df_res, aes(x = reorder(name_territorio, proporcion), y = proporcion)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = scales::percent(proporcion, accuracy = 0.1)),
                hjust = -0.2, size = 3) +
      coord_flip() +
      labs(
        x     = "Departamento",
        y     = "Proporción",
        title = paste("Proporción de locales con", input$filtro_variable, "a nivel Dpto")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                         expand = expansion(mult = c(0, 0.1)))
  })
  
  # --------------------------------------------------------------------------------
  # Dibujar bordes seleccionados (Depto / Prov / Dist)
  # --------------------------------------------------------------------------------
  get_depto_poly <- function(sel_depto) {
    if (is.null(sel_depto) || sel_depto == lbl_all) return(NULL)
    dpto_ids <- data %>%
      filter(DPTO == sel_depto) %>%
      pull(FIRST_IDDP) %>%
      unique()
    departamentos %>% filter(FIRST_IDDP %in% dpto_ids)
  }
  
  get_prov_poly <- function(sel_depto, sel_prov) {
    if (is.null(sel_prov) || sel_prov == lbl_all) return(NULL)
    
    if (!is.null(sel_depto) && sel_depto != lbl_all) {
      data_fil <- data %>% filter(DPTO == sel_depto, PROV == sel_prov)
    } else {
      data_fil <- data %>% filter(PROV == sel_prov)
    }
    prov_ids <- unique(data_fil$FIRST_IDPR)
    provincias %>% filter(FIRST_IDPR %in% prov_ids)
  }
  
  get_dist_poly <- function(sel_depto, sel_prov, sel_dist) {
    if (is.null(sel_dist) || sel_dist == lbl_all) return(NULL)
    
    data_fil <- data
    if (!is.null(sel_depto) && sel_depto != lbl_all) {
      data_fil <- data_fil %>% filter(DPTO == sel_depto)
    }
    if (!is.null(sel_prov) && sel_prov != lbl_all) {
      data_fil <- data_fil %>% filter(PROV == sel_prov)
    }
    data_fil <- data_fil %>% filter(DIST == sel_dist)
    
    dist_ids <- unique(data_fil$IDDIST)
    distritos %>% filter(IDDIST %in% dist_ids)
  }
  
  observeEvent(list(input$sel_depto, input$sel_prov, input$sel_dist), {
    leafletProxy("mapa") %>%
      clearGroup("seleccion")
    
    poly_depto <- get_depto_poly(input$sel_depto)
    poly_prov  <- get_prov_poly(input$sel_depto, input$sel_prov)
    poly_dist  <- get_dist_poly(input$sel_depto, input$sel_prov, input$sel_dist)
    
    if (!is.null(poly_depto) && nrow(poly_depto) > 0) {
      leafletProxy("mapa") %>%
        addPolygons(
          data   = poly_depto,
          group  = "seleccion",
          fill   = FALSE,
          color  = "blue",
          weight = 2,
          opacity= 1
        )
    }
    if (!is.null(poly_prov) && nrow(poly_prov) > 0) {
      leafletProxy("mapa") %>%
        addPolygons(
          data   = poly_prov,
          group  = "seleccion",
          fill   = FALSE,
          color  = "green",
          weight = 2,
          opacity= 1
        )
    }
    if (!is.null(poly_dist) && nrow(poly_dist) > 0) {
      leafletProxy("mapa") %>%
        addPolygons(
          data   = poly_dist,
          group  = "seleccion",
          fill   = FALSE,
          color  = "red",
          weight = 2,
          opacity= 1
        )
    }
  })
}

# --------------------------------------------------------------------------------
# 8) Lanzar la aplicación
# --------------------------------------------------------------------------------
shinyApp(ui, server)
