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
		fluidRow(
			### UpSet plot box ----
			bs4Dash::box(
				width = 12,
				title = shiny::h3("test box"),
				solidHeader = FALSE,
				status = "danger",
				# h4(textOutput("upset_plot_selected")),
				#"Box body",
				id = ns("testing_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				# #actionButton("reset_selection", "Reset Selection"),
				# shiny::verbatimTextOutput(ns("direction_filter")),
				# shiny::verbatimTextOutput(ns("comparisons")),
				# shiny::verbatimTextOutput(ns("ontologies")),
				# shiny::verbatimTextOutput(ns("ora_pvalue")),
				# 
				# shiny::verbatimTextOutput(ns("go_results_selected")),
				# 
				# # shiny::tableOutput(ns("go_results_filtered_tab")),
				# # shiny::tableOutput(ns("comb_GO_result_tibble")),
				# shiny::tableOutput(ns("go_results_selected_tab")),
				# 
				# shiny::verbatimTextOutput(ns("go_results_selected_genes")),
				# shiny::tableOutput(ns("selected_vst_mat")),
				shiny::tableOutput(ns("filtered_DGE_results_tab"))
				
			)
		),
		fluidRow(
			bs4Dash::box(
				width = 6,
				title = shiny::h3("Volcano Plot"),
				solidHeader = FALSE,
				status = "primary",
				# h4(textOutput("upset_plot_selected")),
				#"Box body",
				id = ns("volcano_plot_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				#actionButton("reset_selection", "Reset Selection"),
				shiny::plotOutput(ns("volcano_plot"))
			),
			### Venn plot box ----
			bs4Dash::box(
				width = 6,
				title = shiny::h3("Heatmap"),
				solidHeader = FALSE,
				status = "primary",
				#"Box body",
				id = ns("heatmap_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				shiny::plotOutput(ns("heatmap"))
			)
		),
		### DGE results tabset output ----
		fluidRow(
			bs4Dash::box(
				width = 12,
				title = shiny::h3("GO terms enriched"),
				solidHeader = FALSE,
				status = "primary",
				#"Box body",
				id = ns("ora_reactable_box"),
				collapsible = TRUE,
				closable = FALSE,
				maximizable = TRUE,
				#reactable::reactableOutput(ns("ora_reactable"))
				reactable::reactableOutput("ora_reactable")
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
			#textOutput("n"),
			#textOutput("venn_click_names"),
			#verbatimTextOutput("venn_click_names"),
			#textOutput("mode"),
			
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
			
			shiny::textInput(
				ns("species"), "Species", value = "Homo Sapiens",
				placeholder =  "Homo Sapiens"
			),
			shiny::numericInput(
				ns("deg_pvalue"), "p-value threshold DGE", value = 0.05,
				min = 0, max = 1, step = 0.001
			),
			shiny::numericInput(
				ns("ora_pvalue"), "p-value threshold ORA", value = 0.05,
				min = 0, max = 1, step = 0.001
			),
			shinyWidgets::radioGroupButtons(
				inputId = ns("directional"),
				label = "Direction of Change", 
				selected = "Either",
				choices = c("Up-regulated", "Either", "Down Regulated"),
				status = "primary"
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
		
		output$direction_filter <- renderText(direction_filter())
		direction_filter <- reactive({
			switch(
				input$directional,
				# "Up-regulated" = \(x) dplyr::filter(x, .data$log2FoldChange > 0),
				# "Either" = \(x) dplyr::filter(x),
				# "Down Regulated" = \(x) dplyr::filter(x, .data$log2FoldChange < 0)
				"Up-regulated" = "Up",
				"Either" = "either",
				"Down Regulated" = "Down"
			)
		})
		
		# ontologies <- reactive({
		# 	req(input$comb_GO_result_tibble$datapath)
		# 	unique(comb_GO_result_tibble()$ont)
		# })
		
		output$comparisons <- renderText(input$comparisons)
		output$ontologies <- renderText(input$ontologies)
		output$ora_pvalue <- renderText(input$ora_pvalue)
		
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
		
		output$go_results_filtered_tab <- renderTable(
			head(go_results_filtered()) %>% dplyr::select(-geneID)
		)
		output$comb_GO_result_tibble <- renderTable(
			head(comb_GO_result_tibble()) %>% dplyr::select(-geneID)
		)
		
		output$ora_reactable <- reactable::renderReactable({
			go_results_filtered() %>%
				GO_ORA_RT(onClick = "select", selection = "multiple")
		})
		
		go_results_selected <- reactive({
			get_go_results_selected(
				go_results_filtered(),
				1:10
				# reactable::getReactableState( #go_results_RT
				# 	outputId = ns("ora_reactable"), name = "selected"
				# )
			)
		})
		
		output$go_results_selected <- renderText({
			state <- reactable::getReactableState(
				#ns("ora_reactable"),
				"ora_reactable",
				"selected"
			)
			state
		})
		
		output$go_results_selected_tab <- renderTable({
			go_results_selected() %>% head()
		})
		
		
		output$go_results_selected_genes <- renderText(go_results_selected_genes())
		go_results_selected_genes <- reactive({
			get_go_results_selected_genes(go_results_selected())
		})
		
		output$filtering_ui <- renderUI({
			#req(input$results_annotated_min_cov_grp)
			req(input$results_annotated_min_cov_grp$datapath)
			req(input$comb_GO_result_tibble$datapath)
			
			min_lfc <- floor(
				min(results_annotated_min_cov_grp()$log2FoldChange)
			)
			max_lfc <- ceiling(
				max(results_annotated_min_cov_grp()$log2FoldChange)
			)
			
			all_comparisons <- unique(
				results_annotated_min_cov_grp()$comparison
			)
			
			ontologies <- unique(comb_GO_result_tibble()$ont)
			
			tagList(
				numericInput(
					ns("lfc_down"),
					"Down-regulated threshold log2(Fold Change)", value = -1,
					#min = min_lfc(), max = max_lfc(), step = 0.1
					min = min_lfc, max = max_lfc, step = 0.1
				),
				numericInput(
					ns("lfc_up"),
					"Up-regulated threshold log2(Fold Change)", value = 1,
					#min = min_lfc(), max = max_lfc(), step = 0.1
					min = min_lfc, max = max_lfc, step = 0.1
				),
				selectizeInput(
					ns("comparisons"), "Comparisons", multiple = TRUE,
					# selected = head(all_comparisons(), 2),
					# choices = all_comparisons()
					selected = utils::head(all_comparisons, 2),
					choices = all_comparisons
				),
				selectizeInput(
					ns("ontologies"), "Ontologies", multiple = TRUE,
					# selected = head(all_comparisons(), 2),
					# choices = all_comparisons()
					selected = ontologies,
					choices = ontologies
				)
			)
		})
		
		output$filtered_DGE_results_tab <- renderTable(head(filtered_DGE_results()))
		filtered_DGE_results <- reactive({
			req(input$results_annotated_min_cov_grp$datapath)
			filter_DGE_results(
				results_annotated_min_cov_grp(),
				input$comparisons,
				go_results_selected_genes(),
				input$deg_pvalue, input$lfc_down, input$lfc_up
			)
		})
		
		DGE_with_selected_GO_results <- reactive({
			req(input$results_annotated_min_cov_grp$datapath)
			req(input$comb_GO_result_tibble$datapath)
			add_selected_GO_results(
				#results_annotated_min_cov_grp,
				filtered_DGE_results(),
				go_results_selected()
			)
		})
		
		selected_GO_results_labelled <- reactive({
			req(input$comb_GO_result_tibble$datapath)
			collapse_selected_GO_results_to_label(
				DGE_with_selected_GO_results()
			)
		})
		
		
		output$selected_vst_mat <- renderTable(
			as.data.frame(t(head(selected_vst_mat())))
		)
		selected_vst_mat <- reactive({
			req(input$vst_counts_anno$datapath)
			req(input$comb_GO_result_tibble$datapath)
			counts_selection(
				vst_counts_anno(),
				#unlist(input$comparisons), 
				input$comparisons, 
				# c("BAT_AKO_vs_BAT_WT", "BAT_KO_vs_BAT_WT"),
				# c(
				# 	"Cox6c", "Ldhd", "Ndufa4", "Atp5e", "Nat8l", "Cox8a",
				# 	"Cox6a1", "Crls1", "Ndufb8", "Ndufa1", "Suox", "Sdha"
				# )
				go_results_selected_genes()
			)
		})
		
		## Dark/light Mode toggle state ----
		output$mode <- reactive(input$dark_mode)
		
		output$volcano_plot <- shiny::renderPlot({
			volcano_plotter(selected_GO_results_labelled())
		})

		output$heatmap <- shiny::renderPlot({
			heatmap_plotter(selected_vst_mat())
		})
		
	})
}
    
## To be copied in the UI
# mod_exploRGO_ui("exploRGO_ui_1")
    
## To be copied in the server
# mod_exploRGO_server("exploRGO_ui_1")
