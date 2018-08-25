read.csv.columns <- function(file.name, column.types, max.line.length=16384, has.header=TRUE) 
{
    .Call('RReadCSVColumns', file.name, column.types, max.line.length, has.header, PACKAGE = 'readcsvcolumns')
}

