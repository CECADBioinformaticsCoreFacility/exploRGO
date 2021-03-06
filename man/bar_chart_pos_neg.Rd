% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_exploRGO_fct_exploRGO.R
\name{bar_chart_pos_neg}
\alias{bar_chart_pos_neg}
\title{bar_chart_pos_neg}
\usage{
bar_chart_pos_neg(
  label,
  value,
  max_value = 1,
  height = "16px",
  pos_fill = "#005ab5",
  neg_fill = "#dc3220"
)
}
\arguments{
\item{label}{label text}

\item{value}{value to plot}

\item{max_value}{maximum value in the column}

\item{height}{height of the bar}

\item{pos_fill}{positive values fill colour}

\item{neg_fill}{negative values fill colour}
}
\value{
html plot
}
\description{
directional bar charts centered at 0 for reactable 'sparklin' plots,
discrete up and down colors
}
