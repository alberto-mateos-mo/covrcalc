#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # List the first level UI elements here 
    shinyMobile::f7Page(
      title = "Covid risks",
      shinyMobile::f7SingleLayout(
        navbar = shinyMobile::f7Navbar(
          title = "Calculadora de riesgo de hospitalización"
        ),
        mod_calc_ui("calc_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'covrcalc'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

