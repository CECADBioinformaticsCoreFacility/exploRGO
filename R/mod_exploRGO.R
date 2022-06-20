#' exploRGO UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput h3 fluidRow
#' @importFrom bs4Dash box
#' @importFrom reactable reactableOutput
#' @importFrom dplyr %>%
mod_exploRGO_ui_body <- function(id){
	ns <- NS(id)
	bs4Dash::dashboardBody(
		# output body ----
		# DEBUG OUTPUTS
		# fluidRow(
		# ## DEBUG BOX----
		# 	bs4Dash::box(
		# 		width = 12,
		# 		title = shiny::h3("test box"),
		# 		solidHeader = FALSE,
		# 		status = "danger",
		# 		# h4(textOutput("upset_plot_selected")),
		# 		#"Box body",
		# 		id = ns("testing_box"),
		# 		collapsible = TRUE,
		# 		closable = FALSE,
		# 		maximizable = TRUE,
		# 		# #actionButton("reset_selection", "Reset Selection"),
		# 		# shiny::verbatimTextOutput(ns("direction_filter")),
		# 		# shiny::verbatimTextOutput(ns("comparisons")),
		# 		# shiny::verbatimTextOutput(ns("ontologies")),
		# 		# shiny::verbatimTextOutput(ns("ora_pvalue")),
		# 		# 
		# 		# shiny::verbatimTextOutput(ns("go_results_selected"))# ,
		# 		# 
		# 		# # shiny::tableOutput(ns("go_results_filtered_tab")),
		# 		# # shiny::tableOutput(ns("comb_GO_result_tibble")),
		# 		# shiny::tableOutput(ns("go_results_selected_tab")),
		# 		# 
		# 		# shiny::verbatimTextOutput(ns("go_results_selected_genes")),
		# 		# shiny::tableOutput(ns("selected_vst_mat")),
		# 		# shiny::tableOutput(ns("filtered_DGE_results_tab"))
		# 		
		# 	)
		# ),
		fluidRow(
			bs4Dash::box(
				## volcano box----
				width = 6,
				title = shiny::h3("Volcano Plot"),
				solidHeader = FALSE,
				status = "primary",
				id = ns("volcano_plot_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				shiny::plotOutput(ns("volcano_plot")),
				sidebar = bs4Dash::boxSidebar(
					id = "volcano_sidebar",
					shiny::numericInput(
						ns("deg_pvalue"), "p-value threshold DGE",
						value = 0.05, min = 0, max = 1, step = 0.001
					),
					shiny::uiOutput(ns("filtering_deg_ui"))
				)
			),
			## Heatmap plot box ----
			bs4Dash::box(
				width = 6,
				title = shiny::h3("Heatmap"),
				solidHeader = FALSE,
				status = "primary",
				id = ns("heatmap_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				shiny::plotOutput(ns("heatmap")),
				sidebar = bs4Dash::boxSidebar(
					id = "heatmap_sidebar",
					shinyWidgets::switchInput(
						inputId = ns("show_all"),
						"Show all samples",
						value = FALSE
					),
					shinyWidgets::switchInput(
						inputId = ns("group_means"),
						"Take Mean of Groups",
						value = TRUE
					)
				)
			)
		),
		## GO terms Entrichment result table output ----
		fluidRow(
			bs4Dash::box(
				width = 12,
				title = shiny::h3("GO terms enriched"),
				solidHeader = FALSE,
				status = "primary",
				id = ns("ora_reactable_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				"Select terms to plot genes",
				reactable::reactableOutput(ns("ora_reactable")),
				sidebar = bs4Dash::boxSidebar(
					id = "go_ora_table_sidebar",
					shinyWidgets::radioGroupButtons(
						inputId = ns("directional"),
						label = "Direction of Change", 
						selected = "Either",
						choices = c("Up-regulated", "Either", "Down Regulated"),
						status = "primary"
					),
					shiny::numericInput(
						ns("ora_pvalue"), "p-value threshold ORA",
						value = 0.05, min = 0, max = 1, step = 0.001
					)
				)
			)
		)
	)
}
 
#' exploRGO UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fileInput textInput numericInput
#' @importFrom bs4Dash bs4DashControlbar controlbarItem
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom dplyr %>%
mod_exploRGO_ui_controlbar <- function(id){
	ns <- NS(id)
	## Right sidebar ----
	bs4Dash::bs4DashControlbar(
		id = ns("controlbar"),
		collapsed = FALSE,
		pinned = TRUE,
		overlay = FALSE,
		### UI for main Inputs ----
		bs4Dash::controlbarItem(
			title = "Select Comparisons",
			shiny::fileInput(
				ns("results_annotated_min_cov_grp"),
				"DGE results", multiple = FALSE, accept = ".csv"
			),
			shiny::fileInput(
				ns("comb_GO_result_tibble"),
				"GO ORA results", multiple = FALSE, accept = ".csv"
			),
			shiny::fileInput(
				ns("vst_counts_anno"),
				"counts (vst)", multiple = FALSE, accept = ".tsv"
			),
			shiny::uiOutput(ns("filtering_ui")),
			shiny::uiOutput(ns("set_selector"))
			
		)
	)
}
   
#' exploRGO Server Functions
#'
#' @noRd 
#' @importFrom shiny renderUI moduleServer reactive tagList numericInput selectizeInput req
#' @importFrom readr read_csv
#' @importFrom dplyr %>%
#' @importFrom reactable getReactableState
#' @importFrom utils head
mod_exploRGO_server <- function(id) {
	moduleServer( id, function(input, output, session){
		ns <- session$ns
		# Server ----
		# File processing ----
		options(shiny.maxRequestSize = Inf) # Do not limit file size 
		results_annotated_min_cov_grp <- reactive({
			readr::read_csv(
				input$results_annotated_min_cov_grp$datapath,
				show_col_types = FALSE
			)
		})
		comb_GO_result_tibble <- reactive({
			readr::read_csv(
				input$comb_GO_result_tibble$datapath,
				show_col_types = FALSE
			)
		})
		vst_counts_anno <- reactive({
			readr::read_tsv(
				input$vst_counts_anno$datapath,
				show_col_types = FALSE
			)
		})
		
		# Filtering ----
		# output$direction_filter <- renderText(direction_filter())
		direction_filter <- reactive({
			switch(
				input$directional,
				"Up-regulated" = "Up",
				"Either" = "either",
				"Down Regulated" = "Down"
			)
		})
		
		# ontologies <- reactive({
		# 	req(input$comb_GO_result_tibble$datapath)
		# 	unique(comb_GO_result_tibble()$ont)
		# })
		
		# output$comparisons <- renderText(input$comparisons)
		# output$ontologies <- renderText(input$ontologies)
		# output$ora_pvalue <- renderText(input$ora_pvalue)
		
		go_results_filtered <- reactive({
			req(input$comb_GO_result_tibble$datapath)
			go_ora_filters(
				comb_GO_result_tibble(),
				#comparisons, ontologies, "Up", 0.05
				input$comparisons,
				input$ontologies,
				direction_filter(),
				input$ora_pvalue
			) 
		})
		
		
		# ORA table output ----
		
		# output$go_results_filtered_tab <- renderTable(
		# 	head(go_results_filtered()) %>% dplyr::select(-geneID)
		# )
		# output$comb_GO_result_tibble <- renderTable(
		# 	head(comb_GO_result_tibble()) %>% dplyr::select(-geneID)
		# )
		output$ora_reactable <- reactable::renderReactable({
			go_results_filtered() %>%
				GO_ORA_RT(onClick = "select", selection = "multiple")
		})
		
		## selected ORA table rows ----
		go_results_selected <- reactive({
			get_go_results_selected(
				go_results_filtered(),
				#1:10
				req(reactable::getReactableState(
					outputId = "ora_reactable", name = "selected"
				))
			)
		})
		
		# output$go_results_selected <- renderText({
		# 	state <- req(reactable::getReactableState(
		# 		"ora_reactable",
		# 		"selected"
		# 	))
		# 	state
		# })
		
		# output$go_results_selected_tab <- renderTable({
		# 	go_results_selected() %>% head()
		# })
		
		# output$go_results_selected_genes <- renderText(go_results_selected_genes())
		go_results_selected_genes <- reactive({
			req(input$results_annotated_min_cov_grp$datapath)
			get_go_results_selected_genes(go_results_selected())
		})
		
		# UI for selecting Comparisons & Ontologies ----
		output$filtering_ui <- renderUI({
			req(
				input$results_annotated_min_cov_grp$datapath,
				input$comb_GO_result_tibble$datapath
			)
			all_comparisons <- unique(
				results_annotated_min_cov_grp()$comparison
			)
			
			ontologies <- unique(comb_GO_result_tibble()$ont)
			
			tagList(
				selectizeInput(
					ns("comparisons"), "Comparisons", multiple = TRUE,
					selected = utils::head(all_comparisons, 2),
					choices = all_comparisons
				),
				selectizeInput(
					ns("ontologies"), "Ontologies", multiple = TRUE,
					selected = ontologies,
					choices = ontologies
				)
			)
		})
		
		# UI for LFC filtering DEG results to plot ----
		output$filtering_deg_ui <- renderUI({
			req(
				input$results_annotated_min_cov_grp$datapath,
				input$comb_GO_result_tibble$datapath
			)
			
			min_lfc <- floor(
				min(results_annotated_min_cov_grp()$log2FoldChange)
			)
			max_lfc <- ceiling(
				max(results_annotated_min_cov_grp()$log2FoldChange)
			)
			
			tagList(
				numericInput(
					ns("lfc_down"),
					"Down-regulated threshold log2(Fold Change)", value = -1,
					min = min_lfc, max = max_lfc, step = 0.1
				),
				numericInput(
					ns("lfc_up"),
					"Up-regulated threshold log2(Fold Change)", value = 1,
					min = min_lfc, max = max_lfc, step = 0.1
				)
			)
		})
		
		# DEG results filtering ----
		## DEG threshold & comparison filters ----
		# output$filtered_DGE_results_tab <- renderTable(head(filtered_DGE_results()))
		filtered_DGE_results <- reactive({
			req(
				input$deg_pvalue, input$lfc_down, input$lfc_up,
				input$comparisons,
				input$results_annotated_min_cov_grp$datapath
			)
			if(!is.null(go_results_selected_genes())) {
				filter_DGE_results(
					results_annotated_min_cov_grp(),
					input$comparisons,
					go_results_selected_genes(),
					input$deg_pvalue, input$lfc_down, input$lfc_up
				)
			} else {
				NULL
			}
		})
		
		## DEG selected term filters ----
		DGE_with_selected_GO_results <- reactive({
			req(
				input$results_annotated_min_cov_grp$datapath,
				input$comb_GO_result_tibble$datapath
			)
			add_selected_GO_results(
				req(filtered_DGE_results()),
				go_results_selected()
			)
		})
		
		### Pathway labels for DEG plots ----
		selected_GO_results_labelled <- reactive({
			req(input$comb_GO_result_tibble$datapath)
			collapse_selected_GO_results_to_label(
				req(DGE_with_selected_GO_results())
			)
		})
		
		# Count matric subsetting for heatmap ----
		# output$selected_vst_mat <- renderTable(
		# 	as.data.frame(t(head(selected_vst_mat())))
		# )
		selected_vst_mat <- reactive({
			req(
				input$vst_counts_anno$datapath,
				input$comb_GO_result_tibble$datapath
			)
			counts_selection(
				vst_counts_anno(),
				input$comparisons, 
				# c("BAT_AKO_vs_BAT_WT", "BAT_KO_vs_BAT_WT"),
				# c(
				# 	"Cox6c", "Ldhd", "Ndufa4", "Atp5e", "Nat8l", "Cox8a",
				# 	"Cox6a1", "Crls1", "Ndufb8", "Ndufa1", "Suox", "Sdha"
				# )
				go_results_selected_genes(),
				input$group_means,
				input$show_all
			)
		})
		
		## Dark/light Mode toggle state ----
		output$mode <- reactive(input$dark_mode)
		
		# Plot renderers ----
		## volcano plot ----
		output$volcano_plot <- shiny::renderPlot({
			volcano_plotter(req(selected_GO_results_labelled()))
		})
		## heatmap ----
		output$heatmap <- shiny::renderPlot({
			heatmap_plotter(req(selected_vst_mat()))
		})
		
	})
}
    
## To be copied in the UI
# mod_exploRGO_ui("exploRGO_ui_1")
    
## To be copied in the server
# mod_exploRGO_server("exploRGO_ui_1")
