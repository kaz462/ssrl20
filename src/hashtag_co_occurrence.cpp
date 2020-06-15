// [[Rcpp::plugins("cpp11")]]
#include <Rcpp.h>
#include <vector>
#include <string>
#include <unordered_map>

using namespace Rcpp;

inline std::unordered_map<String, int> getHashtagsTable(StringVector &hashtags_unique) {
  std::unordered_map<String, int> table(hashtags_unique.size());
  for (int i = 0; i < hashtags_unique.size(); i++) {
    table[hashtags_unique[i]] = i;
  }
  return table;
}

// [[Rcpp::export]]
NumericMatrix getCoOccurrenceMatrix(List &hashtags_list, StringVector &hashtags_unique) {
  std::unordered_map<String, int> hashtags_table = getHashtagsTable(hashtags_unique);
  NumericMatrix adj(hashtags_table.size());
  for (int i = 0; i < hashtags_list.size(); i++) {
    StringVector hashtags = hashtags_list[i];
    for (int j = 0; j < hashtags.size(); j++) {
      for (int k = 0; k < hashtags.size(); k++) {
        auto idx1 = hashtags_table.find(hashtags[j]);
        auto idx2 = hashtags_table.find(hashtags[k]);
        if (idx1 != hashtags_table.end() && idx2 != hashtags_table.end()) {
          adj(idx1->second, idx2->second) += 1;
        }
      }
    }
  }
  return adj;
}
