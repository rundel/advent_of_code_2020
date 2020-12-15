#include <Rcpp.h>

// [[Rcpp::export]]
int g_cpp(Rcpp::NumericVector x, int n) {

  std::map<int, int> res;

  for (int i = 0; i < x.size()-1; ++i) {
    res[ x[i] ] = i+1;
  }

  int cur_val = x[ x.size() - 1 ];

  for (int i = x.size() - 1; i < n-1; ++i ) {
    int new_val;

    auto f = res.find(cur_val);
    if ( f == res.end() ) {
      new_val = 0;
    } else {
      new_val = i+1 - f->second;
    }

    res[cur_val] = i+1;
    cur_val = new_val;
  }

  return cur_val;
}

// [[Rcpp::export]]
int g2_cpp(Rcpp::NumericVector x, int n) {

  std::vector<int> res(n, -1);

  for (int i = 0; i < x.size()-1; ++i) {
    res[ x[i] ] = i+1;
  }

  int cur_val = x[ x.size() - 1 ];

  for (int i = x.size() - 1; i < n-1; ++i ) {
    int new_val;

    if ( res[cur_val] == -1 ) {
      new_val = 0;
    } else {
      new_val = i+1 - res[cur_val];
    }

    if (cur_val > res.size()) {
      res.resize(cur_val, -1);
    }

    res[cur_val] = i+1;
    cur_val = new_val;
  }

  return cur_val;
}





/*** R
test = c(0,3,6)
input = c(7,12,1,0,16,2)

g_cpp(test, 2020)
g_cpp(input, 2020)

system.time(g2_cpp(test, 2020))
system.time(g2_cpp(input, 2020))

#g_cpp(input, 30000000)
#g2_cpp(input, 30000000)
*/
