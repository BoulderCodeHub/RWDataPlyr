#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
DataFrame test_df(String const col_er = NA_STRING) {
  StringVector v1(4);
  NumericVector v2(4), v3(4);
  DataFrame val;
  int i;
  std::vector<int> row_names(4);
  
  for (int k = 0; k < 4; k++) {
    row_names.at(k) = k + 1;
  }
  
  std::vector<std::string> vv1;
  vv1.push_back("Jan");
  vv1.push_back("Feb");
  vv1.push_back("Mar");
  vv1.push_back("Apr");
  std::vector<double> vv2;
  vv2.push_back(1.0);
  vv2.push_back(2.0);
  vv2.push_back(3.0);
  vv2.push_back(4.0);
  std::vector<double> vv3;
  vv3.push_back(10.10);
  vv3.push_back(20.20);
  vv3.push_back(40.30);
  vv3.push_back(50.50);
  
  v1 = vv1;
  v2 = vv2;
  v3 = vv3;
  
  val = DataFrame::create(
    _["Month"] = v1,
    _["Month_Num"] = v2,
    _["Value"] = v3 
  );
  
  if (col_er != NA_STRING) {
    i = val.findName(col_er);
    val.erase(i);
    val.attr("class") = "data.frame";
    val.attr("row.names") = row_names;
  } else {
    i = -99;
  }
  
  return val;
}
