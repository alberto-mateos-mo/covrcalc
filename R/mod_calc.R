#' calc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_calc_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyMobile::f7Card(
      shinyMobile::f7Select(ns("sexo"), "Seleccione el sexo:", choices = c("HOMBRE", "MUJER")),
      h4("Ingrese la edad:"),
      shinyMobile::f7Stepper(ns("edad"), label = "", value = 30, min = 0, max = 120, step = 1, color = "black", manual = TRUE),
      shinyMobile::f7checkBoxGroup(ns("comor"), "Selecciona las comorbilidades", choices = c("Neumonía", "Diabetes", "EPOC", "Asma", 
                                                                                             "Inmunosupresión", "Hipertensión", "Obesidad",
                                                                                             "Enfermedad Renal Crónica", "Tabaquismo", "Otra"))
    ),
    shinyMobile::f7Card(title = "Resultado",
      plotOutput(ns("resultado"))
    )
  )
}
    
#' calc Server Function
#'
#' @noRd 
mod_calc_server <- function(input, output, session){
  ns <- session$ns
 
  caso <- reactive({
    data.frame(sexo = input$sexo, edad = input$edad, 
               neumonia = ifelse("Neumonía" %in% input$comor, "SI", "NO"), 
               diabetes = ifelse("Diabetes" %in% input$comor, "SI", "NO"), 
               epoc = ifelse("EPOC" %in% input$comor, "SI", "NO"), 
               asma = ifelse("Asma" %in% input$comor, "SI", "NO"), 
               inmusupr = ifelse("Inmunosupresión" %in% input$comor, "SI", "NO"), 
               hipertension = ifelse("Hipertensión" %in% input$comor, "SI", "NO"), 
               otra_com = ifelse("Otra" %in% input$comor, "SI", "NO"), 
               obesidad = ifelse("Obesidad" %in% input$comor, "SI", "NO"),
               renal_cronica = ifelse("Enfermedad Renal Crónica" %in% input$comor, "SI", "NO"), 
               tabaquismo = ifelse("Tabaquismo" %in% input$comor, "SI", "NO"))
  })
  
  output$resultado <- renderPlot({
    df <- data.frame(matrix(nrow=1, ncol = 1))
    names(df) <- c("resultado")
    df$resultado <- c(stats::predict(modelo, caso(), type = "response"))
    df <- df %>%  
      dplyr::mutate(label = scales::percent(resultado))
    
    ggplot2::ggplot(df, ggplot2::aes(ymax = resultado, ymin = 0, xmax = 2, xmin = 1)) +
      ggplot2::geom_rect(ggplot2::aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
      ggplot2::geom_rect(fill = "#7A002B") + 
      ggplot2::coord_polar(theta = "y",start=-pi/2) + ggplot2::xlim(c(0, 2)) + ggplot2::ylim(c(0,2)) +
      ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = label), colour = "#7A002B", size=6.5) +
      ggplot2::geom_text(ggplot2::aes(x=0.5, y=1.5, label= "Probabilidad de hospitalización"), size=4.2)+
      ggplot2::theme_void() +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank()) +
      ggplot2::guides(fill=FALSE) +
      ggplot2::guides(colour=FALSE)
  })
  
}
    
## To be copied in the UI
# mod_calc_ui("calc_ui_1")
    
## To be copied in the server
# callModule(mod_calc_server, "calc_ui_1")
 
