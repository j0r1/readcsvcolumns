#include <Rcpp.h>
#include <string>

using namespace Rcpp;

List ReadCSVColumns(std::string fileName, std::string columnSpec, int maxLineLength, bool hasHeaders, int numThreads);

RcppExport SEXP RReadCSVColumns(SEXP fileName, SEXP columnSpec, SEXP maxLineLength, SEXP hasHeaders, SEXP numThreads) 
{
BEGIN_RCPP

    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        List __result = ReadCSVColumns(Rcpp::as<std::string>(fileName), 
			               Rcpp::as<std::string>(columnSpec),
				       Rcpp::as<int>(maxLineLength),
				       Rcpp::as<bool>(hasHeaders),
				       Rcpp::as<int>(numThreads));
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;

END_RCPP
}

