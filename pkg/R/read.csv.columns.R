read.csv.columns <- function(file.name, column.types="", max.line.length=16384, has.header=TRUE, num.threads=1) 
{
    if (num.threads < 1)
    	num.threads <- detectCores();

    .Call('RReadCSVColumns', file.name, column.types, max.line.length, has.header, num.threads, PACKAGE = 'readcsvcolumns')
}

