#include <Rcpp.h>

// [[Rcpp::export]]
int g(Rcpp::NumericVector x, int n) {

  std::map<int, int> res;

  for (int i = 0; i < x.size()-1; ++i) {
    res[ x[i] ] = i+1;
  }

  int cur_val = x[ x.size() - 1 ];

  for (int i = x.size() - 1; i < n-1; ++i ) {
    int new_val;

    auto x = res.find(cur_val);
    if ( x == res.end() ) {
      new_val = 0;
    } else {
      new_val = i+1 - x->second;
    }

    res[cur_val] = i+1;
    cur_val = new_val;
  }

  return cur_val;
}





/*** R
g(test, 2020)
g(input, 2020)

#g(test, 30000000)
#g(input, 30000000)
*/
