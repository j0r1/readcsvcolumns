\name{read.csv.columns}
\alias{read.csv.columns}
\docType{package}
\title{
	Read a CSV file by specifying the type of each column in advance
}
\description{
	Read a CSV file by specifying the type of each column in advance.
	A column type can specify that integer values should be read,
	real numbers, or strings. It is also possible to specify that
	certain columns should be ignored. 
}
\usage{
read.csv.columns(file.name, column.types="", max.line.length=16384, has.header=TRUE, num.threads=1) 
}
\arguments{
  \item{file.name}{The path to the CSV file which should be read.}
  \item{column.types}{A string with as many characters as columns in the CSV file, each
                      character specifying the type of the column. If left empty, an attempt
		      will be made to guess the type of each column based on the first line
		      containing data.}
  \item{max.line.length}{An upper limit to the length of each line in the CSV file, the
                         default is probably plenty.}
  \item{has.header}{If TRUE, the first line is considered to contain labels for the columns.
                    Set this to FALSE if the CSV file contains only data, no column names.}
  \item{num.threads}{By default, a single processor thread is used to parse the strings into numbers.
                     If this is set to a number larger than one, this amount of threads will
		     be used to parse this data, possibly offering a speedup. If the number
		     is zero or negative, the amount of cores as reported by \code{detectCores}
		     function (from the \code{parallel} package) will be used.}
}
\details{
	The characters in the \code{column.types} string can be the following:
	\itemize{
		\item \code{i}: the column contains integers
		\item \code{r}: the column contains real numbers
		\item \code{s}: the column contains arbitrary strings
    		\item \code{.}: the column should be ignored
	}
}
\value{
	Returns a list in which each entry contains a column of the CSV file. The columns
	that were marked as 'ignored', are not present in this output list.
}

\examples{
	file.name <- system.file( "test.csv", package = "readcsvcolumns" )
	r <- read.csv.columns(file.name, "ri.s")
	r
}

