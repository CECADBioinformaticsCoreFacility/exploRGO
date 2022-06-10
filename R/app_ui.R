#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom shiny h3 tagList
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar
#' @importFrom waiter spin_1
app_ui <- function(request) {
	tagList(
		# Leave this function for adding external resources
		golem_add_external_resources(),
		# Your application UI logic 
		bs4Dash::dashboardPage(
			dark = NULL,
			## Animation on app start ---- 
			# covers initial renderUI lag
			preloader = list(html = waiter::spin_1(), color = "#333e48"),
			header = bs4Dash::dashboardHeader(
				#title = h1("degSets") #: Set Interesctions
				title = shiny::h3("exploRGO")
			),
			## Left Side bar ----- 
			# used for navigation
			sidebar = bs4Dash::dashboardSidebar(
				id = "sidebar",
				disable = TRUE,
				collapsed = TRUE
			),
			body = mod_exploRGO_ui_body("exploRGO_ui_1"),
			controlbar = mod_exploRGO_ui_controlbar("exploRGO_ui_1")
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
      app_title = 'exploRGO'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

