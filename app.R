# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
# install.packages("BiocManager")
# library(BiocManager)
options(repos = BiocManager::repositories())
# BiocManager::install(
# 	c("BiocGenerics", "ComplexHeatmap", "IRanges", "S4Vectors")
# )
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
exploRGO::run_app() # add parameters here (if any)
