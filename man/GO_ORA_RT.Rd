% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_exploRGO_fct_exploRGO.R
\name{GO_ORA_RT}
\alias{GO_ORA_RT}
\title{GO_ORA_RT}
\usage{
GO_ORA_RT(data, height = 900, onClick = "expand", selection = NULL)
}
\arguments{
\item{data}{GO overrepresentation analysis result table derived from clusterProfiler output}

\item{height}{height of the table widget}

\item{onClick}{what action to perform when clicked "expand", or "select"}

\item{selection}{selection type NULL, "single", or "multiple"}
}
\value{
a reactable
}
\description{
GO_ORA_RT
}
