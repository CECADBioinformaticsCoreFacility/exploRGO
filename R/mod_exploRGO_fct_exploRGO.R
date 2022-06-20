# GO ORA Table Formatting Functions ----

#' scale_check_set_default_on_fail
#'
#' Checks to see if a vector with have properties that will be a problem for
#' using it to create a colour scale such as containing infinite values or
#' having a length of less than 3
#' allows the user to set a defaul scale if checks fail
#'
#' @param vec a numeric vector
#' @param default the default range to use in the event the vector fails checks
#'
#' @return a numeric vector
#'
scale_check_set_default_on_fail <- function(vec, default) {
	vecf <- vec[is.finite(vec)]
	# needs at least 3 unique finite values to make a colour scale
	if(length(unique(vecf)) <= 3) { 
		return(default)
	}
	# set and any infinite values to NA which can be handled by the colour scale
	if(any(!is.finite(vec))) {
		vec[is.finite(vec)] <- as.numeric(NA)
	}
	return(vec)
}

#' bar_chart
#'
#' bar charts for reactable 'sparklin' plots, fixed color
#'
#' @param label label text
#' @param width width of the bar
#' @param height height of the bar
#' @param fill colour of the bar
#' @param background background colour of the cell
#'
#' @return html
bar_chart <- function(
	label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL
) {
	bar <- htmltools::div(
		style = list(background = fill, width = width, height = height)
	)
	chart <- htmltools::div(
		style = list(flexGrow = 1, marginLeft = "6px", background = background),
		bar
	)
	htmltools::div(
		style = list(display = "flex", alignItems = "center"), label, chart #"right"
	)
}

#' bar_chart_pos_neg
#'
#' directional bar charts centered at 0 for reactable 'sparklin' plots,
#' discrete up and down colors
#' 
#' @param label label text
#' @param value value to plot
#' @param max_value maximum value in the column
#' @param height height of the bar
#' @param pos_fill positive values fill colour
#' @param neg_fill negative values fill colour
#'
#' @return html plot
#'
#' @importFrom htmltools div tagAppendChild
bar_chart_pos_neg <- function(
	label, value, max_value = 1, height = "16px",
	pos_fill = "#005ab5", neg_fill = "#dc3220"
) {
	neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
	pos_chart <- htmltools::div(style = list(flex = "1 1 0"))
	width <- paste0(abs(value / max_value) * 100, "%")
	
	if (value < 0) {
		bar <- htmltools::div(
			style = list(
				marginLeft = "8px", background = neg_fill, width = width,
				height = height
			)
		)
		chart <- htmltools::div(
			style = list(
				display = "flex", alignItems = "center",
				justifyContent = "flex-end"
			), label, bar
		)
		neg_chart <- htmltools::tagAppendChild(neg_chart, chart)
	} else {
		bar <- htmltools::div(
			style = list(
				marginRight = "8px", background = pos_fill, width = width,
				height = height
			)
		)
		chart <- htmltools::div(
			style = list(display = "flex", alignItems = "center"), bar, label
		)
		pos_chart <- htmltools::tagAppendChild(pos_chart, chart)
	}
	
	htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)
}

#' GO_ORA_RT
#'
#' @param data GO overrepresentation analysis result table derived from clusterProfiler output
#' @param height height of the table widget
#' @param onClick what action to perform when clicked "expand", or "select"
#' @param selection selection type NULL, "single", or "multiple"
#'
#' @return a reactable
#'
#' @importFrom colourScaleR universal_colour_scaler
#' @importFrom reactable reactable colDef
#' @importFrom htmltools div
#' @importFrom dplyr %>%
#' 
GO_ORA_RT <- function(data, height = 900, onClick = "expand", selection = NULL) {
	
	if(is.null(data)) {
		return(list("No results"))
	}
	
	if(nrow(data) == 0) {
		return(list("No results"))
	}
	
	q_scale_val <- scale_check_set_default_on_fail(data$qvalue, seq(0, 1, 0.2))
	qvaluef <- colourScaleR::universal_colour_scaler(
		q_scale_val,
		type = "scico", palette = "hawaii", mode = "closure", direction = 1,
		n_breaks = 9
	)
	
	padj_scale_val <- scale_check_set_default_on_fail(data$p.adjust, seq(0, 1, 0.2))
	p.adjustf <- colourScaleR::universal_colour_scaler(
		padj_scale_val,
		type = "scico", palette = "hawaii", mode = "closure", direction = 1,
		n_breaks = 9
	)
	
	p_scale_val <- scale_check_set_default_on_fail(data$pvalue, seq(0, 1, 0.2))
	pvaluef <- colourScaleR::universal_colour_scaler(
		p_scale_val,
		type = "scico", palette = "hawaii", mode = "closure", direction = 1,
		n_breaks = 9
	)
	
	pc_scale_val <- scale_check_set_default_on_fail(data$percent, seq(0, 1, 0.2))
	percentf <- colourScaleR::universal_colour_scaler(
		pc_scale_val,
		type = "viridis", palette = "viridis", mode = "closure", direction = 1,
		n_breaks = 9
	)
	
	table <- 
		data %>% reactable::reactable(
			details = function(index) {
				htmltools::div(
					paste(
						"Genes: ", paste0(
							strsplit(
								unlist(data[index,"geneID"]),"/"
							)[[1]], collapse = ", "
						)
					)
				)
			},
			columns = list(
				percent = reactable::colDef(
					#format = reactable::colFormat(percent = TRUE, digits = 1)
					cell = function(value) {
						valuef <- sprintf("%.1f%%", value)
						# valuef <- paste0(format(value, digits = 3),"%")
						bar_chart(valuef, width = value, fill = percentf(value))
					}#,
					#align = "right"
				),
				pvalue = reactable::colDef(
					#format = reactable::colFormat(digits = 3),
					cell = function(value) {
						sprintf("%.3e", value)
					},
					style = function(value) {
						colour <- pvaluef(value)
						list(background = colour)
					}
					# bar_style(width = -log10(value))
				),
				p.adjust = reactable::colDef(
					cell = function(value) {
						sprintf("%.3e", value)
					},
					style = function(value) {
						colour <- p.adjustf(value)
						list(background = colour)
					}
				),
				qvalue = reactable::colDef(
					cell = function(value) {
						sprintf("%.3e", value)
					},
					style = function(value) {
						colour <- qvaluef(value)
						list(background = colour)
					}
				),
				GeneRatio = reactable::colDef(
					cell = function(value) {
						sprintf("%.3e", value)
					}#,
					# style = function(value) {
					# 	colour <- qvaluef(value)
					# 	list(background = colour)
					# }
				),
				BgRatio = reactable::colDef(
					cell = function(value) {
						sprintf("%.3e", value)
					}#,
					# style = function(value) {
					# 	colour <- qvaluef(value)
					# 	list(background = colour)
					# }
				),
				geneID = reactable::colDef(show = FALSE)
			),
			showSortable = TRUE,
			searchable = TRUE, pagination = FALSE, highlight = TRUE,
			height = height,
			filterable = TRUE,
			onClick = onClick, selection = selection
		)
	#list(table)
}


#' get_conditions
#'
#' parses comparisons to get conditions
#'
#' @param comparisons a list of comparison strings
#'
#' @return a character vector of experimenta conditions
#'
#' @importFrom dplyr %>%
get_conditions <- function(comparisons) {
	comparisons %>%
		strsplit(split = "_vs_") %>%
		unlist() %>%
		unique()
}
# comparisons %>% get_conditions()comparisons

#' go_ora_filters
#'
#' @param data Gene Ontology (GO) terms over representation analysis (ORA) results derived from clusterProfiler results
#' @param comparisons the conditions to display,
#' from those being compared in the differential gene expression (DGE) analysis on which this ORA is based
#' @param ontologies the ontology(s) to display (BP, MF, CC)
#' @param directions the direction(s) of change in gene expression for which the ORA was performed (Up, Down, Either)
#' @param p_value the p-value cut off for the ORA analysis
#'
#' @return tibble
#'
#' @importFrom dplyr filter arrange %>%
#' @importFrom rlang .data
go_ora_filters <- function(data, comparisons, ontologies, directions, p_value) {
	if(directions == "either") {
		directions <- c("Up", "Down")
	}
	data %>%
		dplyr::filter(
			.data$comparison %in% comparisons, .data$ont %in% ontologies,
			.data$direction %in% directions, .data$pvalue < p_value
		) %>%
		dplyr::arrange(.data$pvalue)
}

# go_results_filtered <- 
# 	comb_GO_result_tibble %>%
# 	go_ora_filters(comparisons,ontologies,"Up",0.05) 
# 
# go_results_filtered %>%
# 	head(20) %>%
# 	GO_ORA_RT(onClick = "select", selection = "multiple")

#' get_go_results_selected
#'
#' @param go_results_filtered the GO ORA results after filtering with
#'  \code{\link{go_ora_filters}} 
#' @param selected_lines the lines from the ORA result table which have been
#' selected by the user
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' go_results_selected <- get_go_results_selected(
#' 	go_results_filtered, c(1,4,5)
#' )
#' }
#' @importFrom dplyr %>% slice mutate select
#' @importFrom tidyr unnest
get_go_results_selected <- function(go_results_filtered, selected_lines) {
	go_results_filtered %>% 
		dplyr::slice(selected_lines) %>%
		dplyr::mutate(geneID = strsplit(.data$geneID, split = "/")) %>%
		dplyr::select(
			symbol = .data$geneID, .data$ont, .data$comparison,
			.data$direction, .data$ID, .data$Description
		) %>%
		tidyr::unnest(.data$symbol)
}


#' get_go_results_selected_genes
#'
#' @param go_results_selected gets just the gene names from the output of
#'  \code{\link{get_go_results_selected}}
#'
#' @return a character vector of gene symbols
#' @importFrom dplyr pull %>%
#' @examples
#' \dontrun{
#' go_results_selected_genes <- get_go_results_selected_genes(
#' 	 go_results_selected
#' )
#' }
get_go_results_selected_genes <- function(go_results_selected) {
	go_results_selected %>% dplyr::pull(.data$symbol) %>% unlist() %>% unique()
}


#' counts_selection
#'
#' @param counts count matrix as tibble with gene_id, SYMBOL columns
#' @param comparisons the comparisons between conditions for which to get the counts
#' @param genes the genes for which to get the counts
#' @param mean_of_repeats if TRUE (default) takes the mean of repeats, 
#' if FALSE shows results for all conditions.
#' @param all_comparisons show all comparisons instead of the selected subset
#'
#' @return matrix
#' 
#' @importFrom dplyr %>% select
#' @importFrom purrr map
#' @importFrom rlang .data
counts_selection <- function(
	counts, comparisons, genes, mean_of_repeats = TRUE, all_comparisons = FALSE
) {
	counts_mat <- counts %>% 
		dplyr::select(-c(.data$gene_id, .data$SYMBOL)) %>%
		data.matrix()
	rownames(counts_mat) <- counts$SYMBOL
	
	selected_conditions <- colnames(counts_mat)
	if(!all_comparisons) {
		conditions <- get_conditions(comparisons)
		conditions_regex <- paste(conditions, collapse = "|")
		selected_conditions <- grepl(conditions_regex, colnames(counts_mat))
	}
	
	counts_mat_selected <- counts_mat[
		genes, selected_conditions
	]
	
	if(mean_of_repeats) {
		counts_mat_selected <- purrr::map(conditions, ~{
			mat <- rowMeans(counts_mat_selected[
				, grepl(.x, colnames(counts_mat_selected)), drop = FALSE
			])
		})
		counts_mat_selected <- do.call("cbind", counts_mat_selected)
		colnames(counts_mat_selected) <- conditions
	}
	
	counts_mat_selected
}

# selected_vst_mat <- counts_selection(
# 	vst_counts_anno, comparisons, go_results_selected_genes
# )

#' filter_DGE_results
#' 
#' filtering fo the differentiall epxressed gene results
#' 
#' @param dge_data differentially expressed gene results from our RNA-seq pipeline
#' @param comparisons the comparisons between conditions to include
#' @param genes the genes to include
#' @param p_value the p-value cut-off
#' @param lfc_down the lower log2 fold change threshold
#' @param lfc_up the upper log2 fold change threshold
#'
#' @return a tibble
#'
#' @importFrom dplyr filter %>%
filter_DGE_results <- function(
	dge_data, comparisons, genes, p_value = 0.05, lfc_down = -1, lfc_up = 1
) {
	dge_data %>%
		dplyr::filter(
			.data$comparison %in% comparisons,
			.data$symbol %in% genes,
			.data$pvalue < p_value, 
			.data$log2FoldChange > lfc_up | .data$log2FoldChange < lfc_down
		)
}

# DGE_results_filtered <- filter_DGE_results(
# 	results_annotated_min_cov_grp,
# 	comparisons, go_results_selected_genes
# )

#' add_selected_GO_results
#'
#' combined the results of \code{\link{filter_DGE_results}} with
#'  \code{\link{add_selected_GO_results}} to allow highlighting of genes
#'   in selected pathways
#'
#' @param DGE_data results of \code{\link{filter_DGE_results}}
#' @param go_results_selected results of \code{\link{add_selected_GO_results}}
#'
#' @return a tibble
#' 
#' @importFrom dplyr %>% group_by ungroup left_join
#' @importFrom tidyr nest
add_selected_GO_results <- function(DGE_data, go_results_selected) {
	dplyr::left_join(
		DGE_data, go_results_selected, by = c("comparison","symbol")
	) %>%
		dplyr::group_by(
			.data$gene_id, .data$baseMean, .data$log2FoldChange, .data$lfcSE,
			.data$pvalue, .data$padj, .data$seqnames,
			.data$start, .data$end, .data$width, .data$strand, .data$gene_name,
			.data$gene_biotype, .data$seq_coord_system, .data$description, 
			.data$symbol, .data$entrezid, .data$comparison
		) %>% 
		tidyr::nest() %>%
		dplyr::ungroup()
}

# DGE_with_selected_GO_results <- add_selected_GO_results(
# 	#results_annotated_min_cov_grp,
# 	DGE_results_filtered,
# 	go_results_selected
# )

#' collapse_selected_GO_results_to_label
#'
#' @param DGE_with_selected_GO_results results of \code{\link{add_selected_GO_results}}
#'
#' @return a tibble
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom purrr map_chr
#' @importFrom rlang .data
#' 
#' @examples 
#' \dontrun{
#' selected_GO_results_labelled <- collapse_selected_GO_results_to_label(
#' 	DGE_with_selected_GO_results
#' )
#' }
collapse_selected_GO_results_to_label <- function(DGE_with_selected_GO_results) {
	DGE_with_selected_GO_results %>%
		dplyr::mutate(
			#n = purrr::map_int(data, nrow),
			selected_pathways = purrr::map_chr(.data$data, ~{
				res <- NULL
				if(nrow(.x) == 1 & is.na(.x[[1, 1]])) {
					res <- NA_character_
				} else if (nrow(.x) > 1) {
					res <- "multiple"
				} else {
					res <- paste0(
						.x$direction, " (", .x$ont, ":", .x$ID, ") ",
						.x$Description
					)
				}
				res
			})
		) %>%
		dplyr::select(-.data$data)
}


#' volcano_plotter
#'
#' @param DGE_data differential gene expression datr from which to make a plot 
#'
#' @return a ggplot2 object
#' 
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs
#' @importFrom latex2exp TeX
#' @importFrom scico scale_color_scico_d
#' @importFrom rlang .data
#' @examples 
#' \dontrun{
#' volcano_plotter(selected_GO_results_labelled)
#' }
volcano_plotter <- function(DGE_data) {
	ggplot2::ggplot(
		DGE_data, ggplot2::aes(.data$log2FoldChange, -log10(.data$pvalue))
	) + 
		ggplot2::geom_point(color = "grey", shape = 1) + 
		ggplot2::geom_point(
			ggplot2::aes(color = .data$selected_pathways), size = 2
		) + 
		scico::scale_color_scico_d() +
		#ggplot2::theme_minimal() + 
		ggplot2::theme_bw() + 
		ggplot2::labs(
			y = latex2exp::TeX("$-log_{10}(p-value)$"),
			x = latex2exp::TeX("$log_{2}(Fold Change)$")
		) + 
		ggplot2::facet_wrap(~.data$comparison)
}


#' heatmap_plotter
#' 
#' @param mat a gene expression matrix
#'
#' @return a ComplexHeatmap Heatmap
#'
#' @examples
#' \dontrun{
#' heatmap_plotter(selected_vst_mat)
#' }
#' 
#' @importFrom ComplexHeatmap Heatmap
heatmap_plotter <- function(mat) {
	#ComplexHeatmap::pheatmap(mat)
	ComplexHeatmap::Heatmap(t(mat), name = "vst")
}

