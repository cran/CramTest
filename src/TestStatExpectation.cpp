#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
float TestStatExpectation(NumericVector sample, NumericVector Regions){
	float n_s = sample.size();
	float inv_n_s = 1/n_s;
	float m = Regions.size();
	Regions.push_back(max(sample));
	float area_u_v;
	float empiricalv1;
	float empiricalv2;
	area_u_v = 0;
	for (int v=0;v<m;v++){
		empiricalv1 = sum(sample<=Regions[v])*inv_n_s;
		empiricalv2 = sum(sample<=Regions[v+1])*inv_n_s;
		area_u_v = area_u_v + (((empiricalv1 - pow(empiricalv1,2)) + (empiricalv2 - pow(empiricalv2,2)))/2)*(Regions[v+1]-Regions[v]);
	}
	return area_u_v;
}
