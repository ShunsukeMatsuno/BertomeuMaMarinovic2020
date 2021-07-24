// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcppeigen_hello_world
Eigen::MatrixXd rcppeigen_hello_world();
RcppExport SEXP _BertomeuMaMarinovic_rcppeigen_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcppeigen_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// rcppeigen_outerproduct
Eigen::MatrixXd rcppeigen_outerproduct(const Eigen::VectorXd& x);
RcppExport SEXP _BertomeuMaMarinovic_rcppeigen_outerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppeigen_outerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcppeigen_innerproduct
double rcppeigen_innerproduct(const Eigen::VectorXd& x);
RcppExport SEXP _BertomeuMaMarinovic_rcppeigen_innerproduct(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppeigen_innerproduct(x));
    return rcpp_result_gen;
END_RCPP
}
// rcppeigen_bothproducts
Rcpp::List rcppeigen_bothproducts(const Eigen::VectorXd& x);
RcppExport SEXP _BertomeuMaMarinovic_rcppeigen_bothproducts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcppeigen_bothproducts(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_BertomeuMaMarinovic_rcppeigen_hello_world", (DL_FUNC) &_BertomeuMaMarinovic_rcppeigen_hello_world, 0},
    {"_BertomeuMaMarinovic_rcppeigen_outerproduct", (DL_FUNC) &_BertomeuMaMarinovic_rcppeigen_outerproduct, 1},
    {"_BertomeuMaMarinovic_rcppeigen_innerproduct", (DL_FUNC) &_BertomeuMaMarinovic_rcppeigen_innerproduct, 1},
    {"_BertomeuMaMarinovic_rcppeigen_bothproducts", (DL_FUNC) &_BertomeuMaMarinovic_rcppeigen_bothproducts, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_BertomeuMaMarinovic(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
