# --------------------------------------------------------------------------------
# 1) Carga de librerías
# --------------------------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(ggplot2)

# --------------------------------------------------------------------------------
# 2) Rutas a los archivos y lectura
# --------------------------------------------------------------------------------
data_path <- "data"  # Carpeta relativa donde están los datos

shapefile_departamentos <- file.path(data_path, "Departamentos.geojson")
shapefile_provincias    <- file.path(data_path, "Provincias.geojson")
shapefile_distritos     <- file.path(data_path, "Distritos.geojson")
data_excel              <- file.path(data_path, "paraShiny2.xlsx")

# Cargar shapefiles
cat("Cargando shapefiles...\n")
departamentos <- st_read(shapefile_departamentos, quiet = TRUE)
provincias    <- st_read(shapefile_provincias, quiet = TRUE)
distritos     <- st_read(shapefile_distritos, quiet = TRUE) %>%
  filter(!st_is_empty(geometry))
cat("Shapefiles cargados correctamente.\n")

# Cargar datos desde Excel
cat("Cargando datos desde Excel...\n")
data <- read_excel(data_excel, sheet = "preparando")
cat("Datos cargados correctamente.\n")

# --------------------------------------------------------------------------------
# 3) Procesamiento de datos
# --------------------------------------------------------------------------------
departamentos <- departamentos %>% mutate(FIRST_IDDP = as.character(FIRST_IDDP))
provincias    <- provincias %>% mutate(FIRST_IDPR = as.character(FIRST_IDPR))
distritos     <- distritos %>% mutate(IDDIST = as.character(IDDIST))

data <- data %>%
  mutate(
    FIRST_IDDP  = as.character(FIRST_IDDP),
    FIRST_IDPR  = as.character(FIRST_IDPR),
    IDDIST      = as.character(IDDIST),
    DPTO        = as.character(DPTO),
    PROV        = as.character(PROV),
    DIST        = as.character(DIST),
    FUENTE_AGUA  = trimws(FUENTE_AGUA),
    TIPO_DESAGUE = trimws(TIPO_DESAGUE),
    FUENTE_ELEC  = trimws(FUENTE_ELEC),
    servbasic    = trimws(servbasic),
    tipo_gest    = as.character(tipo_gest),
    AREA_CENSO2  = as.character(AREA_CENSO2)
  )

# Mapas para los datos
AGUA_MAPPING <- c(
  "Red pública"        = "Agua_Red_pública",
  "Camión cisterna"    = "Agua_Camión_cisterna",
  "Captación lluvia"   = "Agua_Captación_lluvia",
  "No tiene"           = "Agua_No_tiene",
  "Otro"               = "Agua_Otro",
  "Pilón público"      = "Agua_Pilón_público",
  "Pozo"               = "Agua_de_pozo",
  "Río, acequia"       = "Agua_de_río_acequia"
)

DESAGUE_MAPPING <- c(
  "Red pública"               = "Desague_Red_pública",
  "Río o acequia o canal"     = "Desague_Río_acequia_o_canal",
  "Tanque séptico"            = "Desague_tanque_séptico",
  "Biodigestor"               = "Desague_Biodigestor",
  "Pozo sin tratamiento"      = "Desague_Pozo_sin_tratamiento",
  "UBSC"                      = "Desague_ubsc",
  "Otro"                      = "Desague_Otro",
  "No tiene"                  = "Desague_No_tiene"
)

ELECTRICIDAD_MAPPING <- c(
  "Red pública"               = "Electricidad_Red_pública",
  "Motor de municipio"        = "Electricidad_motor_de_municipio",
  "Motor de la comunidad"     = "Electricidad_motor_de_comunidad",
  "Motor del local educativo" = "Electricidad_motor_de_local_educativo",
  "Panel solar"               = "Electricidad_Panel_solar",
  "Energía eólica"            = "Electricidad_eolica",
  "Otro"                      = "Electricidad_Otro",
  "No tiene"                  = "Electricidad_No_tiene"
)

VAR_MAPPING <- list(
  "FUENTE_AGUA"   = AGUA_MAPPING,
  "TIPO_DESAGUE"  = DESAGUE_MAPPING,
  "FUENTE_ELEC"   = ELECTRICIDAD_MAPPING
)

# Crear columnas binarias 0/1
for (variable in names(VAR_MAPPING)) {
  mapping <- VAR_MAPPING[[variable]]
  for (cat_label in names(mapping)) {
    col_name <- mapping[cat_label]
    data[[col_name]] <- ifelse(data[[variable]] == cat_label, 1, 0)
  }
}


# --------------------------------------------------------------------------------
# 5) Interfaz de usuario
# --------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Locales 2023"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa de Calor", tabName="heatmap", icon=icon("globe")),
      
      radioButtons(
        inputId  = "nivel",
        label    = "Seleccionar nivel geográfico:",
        choices  = c("Departamento","Provincia","Distrito"),
        selected = "Departamento",
        inline   = TRUE
      ),
      
      selectInput("variable","Seleccionar variable de interés:",
                  choices=c("FUENTE_AGUA","TIPO_DESAGUE","FUENTE_ELEC","servbasic")),
      uiOutput("filtros_variables"),
      
      selectInput("tipo_gest","Tipo de Gestión:",
                  choices=c("Seleccionar todas","Pública","Privada"),
                  selected="Seleccionar todas"),
      
      selectInput("area_censo2","Tipo de Área:",
                  choices=c("Seleccionar todas","Urbano","Rural"),
                  selected="Seleccionar todas"),
      
      pickerInput(
        inputId="sel_depto",
        label="Departamento:",
        choices=NULL,
        selected=NULL,
        multiple=FALSE,
        options=list(`live-search`=TRUE)
      ),
      
      pickerInput(
        inputId="sel_prov",
        label="Provincia:",
        choices=NULL,
        selected=NULL,
        multiple=FALSE,
        options=list(`live-search`=TRUE)
      ),
      
      pickerInput(
        inputId="sel_dist",
        label="Distrito:",
        choices=NULL,
        selected=NULL,
        multiple=FALSE,
        options=list(`live-search`=TRUE)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName="heatmap",
        
        fluidRow(
          column(width=12,
                 leafletOutput("mapa", height="550px")
          )
        ),
        
        fluidRow(
          column(width=12,
                 div(
                   style="height: 25vh; overflow-x: auto; margin-top:10px; border: 1px solid #ccc; padding: 5px;",
                   plotOutput("grafico_barras", height="100%")
                 )
          )
        )
      )
    )
  )
)

# --------------------------------------------------------------------------------
# 6) Lógica del servidor
# --------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # A) Preparación de "todos" para deptos, provs, dists
  all_dptos <- sort(unique(data$DPTO))
  all_provs <- sort(unique(data$PROV))
  all_dists <- sort(unique(data$DIST))
  lbl_all <- "Seleccionar todos"
  
  prepend_all <- function(x) c(lbl_all, x)
  
  # Inicializamos combos con "Seleccionar todos"
  updatePickerInput(session,"sel_depto",
                    choices=prepend_all(all_dptos),
                    selected=lbl_all)
  updatePickerInput(session,"sel_prov",
                    choices=prepend_all(all_provs),
                    selected=lbl_all)
  updatePickerInput(session,"sel_dist",
                    choices=prepend_all(all_dists),
                    selected=lbl_all)
  
  # Encadenar Deptos -> Provincias -> Distritos
  observeEvent(input$sel_depto,{
    req(input$sel_depto)
    
    if(input$sel_depto==lbl_all){
      new_provs <- all_provs
    } else {
      new_provs <- data %>%
        filter(DPTO==input$sel_depto) %>%
        pull(PROV) %>%
        unique() %>%
        sort()
    }
    
    updatePickerInput(session,"sel_prov",
                      choices=prepend_all(new_provs),
                      selected=lbl_all)
    
    updatePickerInput(session,"sel_dist",
                      choices=prepend_all(all_dists),
                      selected=lbl_all)
  })
  
  observeEvent(input$sel_prov,{
    req(input$sel_prov)
    
    if(input$sel_prov==lbl_all){
      new_dists <- if(input$sel_depto==lbl_all){
        all_dists
      } else {
        data %>%
          filter(DPTO==input$sel_depto) %>%
          pull(DIST) %>%
          unique() %>%
          sort()
      }
    } else {
      if(input$sel_depto==lbl_all){
        new_dists <- data %>%
          filter(PROV==input$sel_prov) %>%
          pull(DIST) %>%
          unique() %>%
          sort()
      } else {
        new_dists <- data %>%
          filter(DPTO==input$sel_depto,PROV==input$sel_prov) %>%
          pull(DIST) %>%
          unique() %>%
          sort()
      }
    }
    
    updatePickerInput(session,"sel_dist",
                      choices=prepend_all(new_dists),
                      selected=lbl_all)
  })
  
  # UI para variable
  output$filtros_variables<-renderUI({
    req(input$variable)
    var_elegida<-input$variable
    
    valores_unicos<-sort(unique(data[[var_elegida]]))
    valor_por_defecto<-if(length(valores_unicos)>0) valores_unicos[1] else NULL
    
    selectInput("filtro_variable",
                label=paste("Seleccionar",var_elegida,":"),
                choices=valores_unicos,
                selected=valor_por_defecto,
                multiple=FALSE)
  })
  
  # --- datosMapa: depende de input$nivel ---
  datosMapa <- reactive({
    req(input$nivel,input$variable,input$filtro_variable)
    
    data_f<-data
    if(input$tipo_gest!="Seleccionar todas"){
      data_f<-data_f%>%filter(tipo_gest==input$tipo_gest)
    }
    if(input$area_censo2!="Seleccionar todas"){
      data_f<-data_f%>%filter(AREA_CENSO2==input$area_censo2)
    }
    
    # ID y nombre según input$nivel
    if(input$nivel=="Departamento"){
      join_col_df<-"FIRST_IDDP"
      name_col   <-"DPTO"
    } else if(input$nivel=="Provincia"){
      join_col_df<-"FIRST_IDPR"
      name_col   <-"PROV"
    } else {
      join_col_df<-"IDDIST"
      name_col   <-"DIST"
    }
    
    # Denominador
    df_total<-data_f%>%
      group_by(.data[[join_col_df]])%>%
      summarise(total_locales=n(),.groups="drop")
    
    # Numerador
    var_actual<-input$variable
    cat_selecc<-input$filtro_variable
    
    if(var_actual%in%names(VAR_MAPPING)){
      mapping<-VAR_MAPPING[[var_actual]]
      selected_col<-mapping[cat_selecc]
      if(is.na(selected_col)){
        df_variable<-data_f%>%
          group_by(.data[[join_col_df]])%>%
          summarise(n_cumple=0,.groups="drop")
      } else {
        df_variable<-data_f%>%
          group_by(.data[[join_col_df]])%>%
          summarise(n_cumple=sum(.data[[selected_col]],na.rm=TRUE),.groups="drop")
      }
    } else {
      df_variable<-data_f%>%
        filter(.data[[var_actual]]==cat_selecc)%>%
        group_by(.data[[join_col_df]])%>%
        summarise(n_cumple=n(),.groups="drop")
    }
    
    df_prop<-df_total%>%
      left_join(df_variable,by=join_col_df)%>%
      mutate(
        n_cumple=tidyr::replace_na(n_cumple,0),
        proporcion=n_cumple/total_locales
      )
    
    df_names<-data_f%>%
      select(all_of(c(join_col_df,name_col)))%>%
      distinct()%>%
      group_by(.data[[join_col_df]])%>%
      summarise(
        name_territorio=first(.data[[name_col]]),
        .groups="drop"
      )
    
    df_final<-df_prop%>%
      left_join(df_names,by=join_col_df)
    
    df_final
  })
  
  # --- datosDeptoBarras: SIEMPRE DEPARTAMENTOS ---
  datosDeptoBarras <- reactive({
    req(input$variable,input$filtro_variable)
    
    data_f<-data
    if(input$tipo_gest!="Seleccionar todas"){
      data_f<-data_f%>%filter(tipo_gest==input$tipo_gest)
    }
    if(input$area_censo2!="Seleccionar todas"){
      data_f<-data_f%>%filter(AREA_CENSO2==input$area_censo2)
    }
    
    # Forzamos Departamentos
    join_col_df<-"FIRST_IDDP"
    name_col   <-"DPTO"
    
    df_total<-data_f%>%
      group_by(.data[[join_col_df]])%>%
      summarise(total_locales=n(),.groups="drop")
    
    var_actual<-input$variable
    cat_selecc<-input$filtro_variable
    
    if(var_actual%in%names(VAR_MAPPING)){
      mapping<-VAR_MAPPING[[var_actual]]
      selected_col<-mapping[cat_selecc]
      if(is.na(selected_col)){
        df_variable<-data_f%>%
          group_by(.data[[join_col_df]])%>%
          summarise(n_cumple=0,.groups="drop")
      } else {
        df_variable<-data_f%>%
          group_by(.data[[join_col_df]])%>%
          summarise(n_cumple=sum(.data[[selected_col]],na.rm=TRUE),.groups="drop")
      }
    } else {
      df_variable<-data_f%>%
        filter(.data[[var_actual]]==cat_selecc)%>%
        group_by(.data[[join_col_df]])%>%
        summarise(n_cumple=n(),.groups="drop")
    }
    
    df_prop<-df_total%>%
      left_join(df_variable,by=join_col_df)%>%
      mutate(
        n_cumple=tidyr::replace_na(n_cumple,0),
        proporcion=n_cumple/total_locales
      )
    
    df_names<-data_f%>%
      select(all_of(c(join_col_df,name_col)))%>%
      distinct()%>%
      group_by(.data[[join_col_df]])%>%
      summarise(
        name_territorio=first(.data[[name_col]]),
        .groups="drop"
      )
    
    df_final<-df_prop%>%
      left_join(df_names,by=join_col_df)
    
    df_final
  })
  
  # Mapa
  output$mapa<-renderLeaflet({
    req(datosMapa())
    df_prop<-datosMapa()
    
    if(input$nivel=="Departamento"){
      selected_geo<-departamentos
      join_col_sf<-"FIRST_IDDP"
      join_col_df<-"FIRST_IDDP"
    } else if(input$nivel=="Provincia"){
      selected_geo<-provincias
      join_col_sf<-"FIRST_IDPR"
      join_col_df<-"FIRST_IDPR"
    } else {
      selected_geo<-distritos
      join_col_sf<-"IDDIST"
      join_col_df<-"IDDIST"
    }
    
    selected_geo<-selected_geo%>%
      left_join(df_prop,by=setNames(join_col_df,join_col_sf))
    
    if(!"proporcion"%in%colnames(selected_geo)){
      showNotification("No se pudo calcular la proporción para el mapa.",type="error")
      return(leaflet()%>%addTiles())
    }
    
    vals_proporcion<-selected_geo$proporcion
    vals_validos   <-vals_proporcion[is.finite(vals_proporcion)]
    
    if(length(vals_validos)==0){
      showNotification("No hay valores numéricos para la proporción en la selección actual.",type="warning")
      return(leaflet()%>%addTiles())
    }
    
    rng<-range(vals_validos,na.rm=TRUE)
    if(rng[1]==rng[2]){
      rng<-c(rng[1]-0.01,rng[2]+0.01)
    }
    
    pal<-colorNumeric("YlOrRd",domain=rng,na.color="transparent")
    
    popup_text<-paste0(
      "<strong>Territorio:</strong> ",
      ifelse(is.na(selected_geo$name_territorio),"",selected_geo$name_territorio),
      "<br><strong>Total locales:</strong> ",
      ifelse(is.na(selected_geo$total_locales),0,selected_geo$total_locales),
      "<br><strong>Cumplen variable:</strong> ",
      ifelse(is.na(selected_geo$n_cumple),0,selected_geo$n_cumple),
      "<br><strong>Proporción:</strong> ",
      ifelse(is.na(selected_geo$proporcion),0,round(selected_geo$proporcion*100,2)),"%"
    )
    
    leaflet(selected_geo)%>%
      addTiles()%>%
      addPolygons(
        fillColor=~pal(proporcion),
        fillOpacity=0.7,
        color="black",
        weight=0.5,
        popup=popup_text
      )%>%
      addLegend(
        pal=pal,
        values=vals_validos,
        title="Proporción",
        position="bottomright"
      )
  })
  
  # Barras: mayor->menor y prevenimos que la más alta se corte
  output$grafico_barras<-renderPlot({
    req(datosDeptoBarras())
    
    df_res<-datosDeptoBarras()
    df_res<-df_res%>%filter(is.finite(proporcion))
    
    # Ordenamos de MAYOR a MENOR: mayor a la izquierda
    df_res<-df_res%>%arrange(desc(proporcion))
    
    # Gráfico
    ggplot(df_res,aes(x=reorder(name_territorio,-proporcion),y=proporcion))+
      geom_col(fill="steelblue")+
      geom_text(aes(label=scales::percent(proporcion,accuracy=0.1)),
                vjust=-0.3,size=3)+
      labs(
        x="Departamento",
        y="Proporción",
        title=paste("Proporción de locales con",input$filtro_variable,"a nivel Dpto")
      )+
      theme_minimal()+
      theme(
        axis.text.x=element_text(angle=45,hjust=1),
        plot.title=element_text(hjust=0.5) # Título centrado
      )+
      # Desactivar recorte: evita que la barra más alta se corte
      coord_cartesian(clip="off")
  })
  
  # BORDES (sin recálculo)
  get_depto_poly<-function(sel_depto){
    if(is.null(sel_depto)||sel_depto==lbl_all)return(NULL)
    dpto_ids<-data%>%
      filter(DPTO==sel_depto)%>%
      pull(FIRST_IDDP)%>%
      unique()
    departamentos%>%
      filter(FIRST_IDDP%in%dpto_ids)
  }
  
  get_prov_poly<-function(sel_depto,sel_prov){
    if(is.null(sel_prov)||sel_prov==lbl_all)return(NULL)
    if(!is.null(sel_depto)&&sel_depto!=lbl_all){
      data_fil<-data%>%
        filter(DPTO==sel_depto,PROV==sel_prov)
    } else {
      data_fil<-data%>%
        filter(PROV==sel_prov)
    }
    prov_ids<-unique(data_fil$FIRST_IDPR)
    provincias%>%
      filter(FIRST_IDPR%in%prov_ids)
  }
  
  get_dist_poly<-function(sel_depto,sel_prov,sel_dist){
    if(is.null(sel_dist)||sel_dist==lbl_all)return(NULL)
    data_fil<-data
    if(!is.null(sel_depto)&&sel_depto!=lbl_all){
      data_fil<-data_fil%>%filter(DPTO==sel_depto)
    }
    if(!is.null(sel_prov)&&sel_prov!=lbl_all){
      data_fil<-data_fil%>%filter(PROV==sel_prov)
    }
    data_fil<-data_fil%>%filter(DIST==sel_dist)
    
    dist_ids<-unique(data_fil$IDDIST)
    distritos%>%
      filter(IDDIST%in%dist_ids)
  }
  
  observeEvent(
    list(input$sel_depto,input$sel_prov,input$sel_dist),
    {
      leafletProxy("mapa")%>%
        clearGroup("seleccion")
      
      poly_depto<-get_depto_poly(input$sel_depto)
      poly_prov <-get_prov_poly(input$sel_depto,input$sel_prov)
      poly_dist <-get_dist_poly(input$sel_depto,input$sel_prov,input$sel_dist)
      
      if(!is.null(poly_depto)&&nrow(poly_depto)>0){
        leafletProxy("mapa")%>%
          addPolygons(
            data=poly_depto,
            group="seleccion",
            fill=FALSE,
            color="blue",
            weight=2,
            opacity=1
          )
      }
      if(!is.null(poly_prov)&&nrow(poly_prov)>0){
        leafletProxy("mapa")%>%
          addPolygons(
            data=poly_prov,
            group="seleccion",
            fill=FALSE,
            color="green",
            weight=2,
            opacity=1
          )
      }
      if(!is.null(poly_dist)&&nrow(poly_dist)>0){
        leafletProxy("mapa")%>%
          addPolygons(
            data=poly_dist,
            group="seleccion",
            fill=FALSE,
            color="red",
            weight=2,
            opacity=1
          )
      }
    }
  )
}

# --------------------------------------------------------------------------------
# 7) Lanzar la aplicación
# --------------------------------------------------------------------------------
shinyApp(ui, server)
