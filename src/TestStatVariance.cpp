#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double TestStatVariance(int n1, int n2, NumericVector sample, NumericVector Regions){
	double n_s = sample.size();
	double m = Regions.size();
	Regions.push_back(max(sample));
	double area_u_v;
	double empiricalv1;
	double empiricalv2;
	double empiricalu1;
	double empiricalu2;
	double area_u1_v;
	double area_u2_v;
	double inv_n_s = 1/n_s;
	area_u_v = 0;
	for (int u=(m-1); u--; ){
		area_u1_v = 0;
		area_u2_v = 0;
		empiricalu1 = sum(sample<=Regions[u+1])*inv_n_s;
		empiricalu2 = sum(sample<=Regions[u+2])*inv_n_s;
		for (int v=(u+1); v--; ){
			empiricalv1 = sum(sample<=Regions[v])*inv_n_s;
			empiricalv2 = sum(sample<=Regions[v+1])*inv_n_s;
			area_u1_v = area_u1_v + ((2*(pow(n1,3)+ pow(n2,3))/(n1*n2*pow((n1+n2),2)))*((empiricalv1 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 4)*empiricalv1*empiricalv1 - 3*empiricalv1*empiricalu1- ((4*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 10)*empiricalv1*empiricalv1*empiricalu1 + 2*empiricalv1*empiricalu1*empiricalu1 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 6)*empiricalv1*empiricalv1*empiricalu1*empiricalu1) + (empiricalv2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 4)*empiricalv2*empiricalv2 - 3*empiricalv2*empiricalu1- ((4*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 10)*empiricalv2*empiricalv2*empiricalu1 + 2*empiricalv2*empiricalu1*empiricalu1 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 6)*empiricalv2*empiricalv2*empiricalu1*empiricalu1))/2)*(Regions[v+1]-Regions[v]);
			area_u2_v = area_u2_v + ((2*(pow(n1,3)+ pow(n2,3))/(n1*n2*pow((n1+n2),2)))*((empiricalv1 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 4)*empiricalv1*empiricalv1 - 3*empiricalv1*empiricalu2- ((4*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 10)*empiricalv1*empiricalv1*empiricalu2 + 2*empiricalv1*empiricalu2*empiricalu2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 6)*empiricalv1*empiricalv1*empiricalu2*empiricalu2) + (empiricalv2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 4)*empiricalv2*empiricalv2 - 3*empiricalv2*empiricalu2- ((4*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 10)*empiricalv2*empiricalv2*empiricalu2 + 2*empiricalv2*empiricalu2*empiricalu2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 6)*empiricalv2*empiricalv2*empiricalu2*empiricalu2))/2)*(Regions[v+1]-Regions[v]);
		}
		empiricalv1 = empiricalu1;
		empiricalv2 = empiricalu2;
		area_u2_v = area_u2_v + ((2*(pow(n1,3)+ pow(n2,3))/(n1*n2*pow((n1+n2),2)))*((empiricalv1 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 4)*empiricalv1*empiricalv1 - 3*empiricalv1*empiricalu2- ((4*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 10)*empiricalv1*empiricalv1*empiricalu2 + 2*empiricalv1*empiricalu2*empiricalu2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 6)*empiricalv1*empiricalv1*empiricalu2*empiricalu2) + (empiricalv2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 4)*empiricalv2*empiricalv2 - 3*empiricalv2*empiricalu2- ((4*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 10)*empiricalv2*empiricalv2*empiricalu2 + 2*empiricalv2*empiricalu2*empiricalu2 + ((2*n1*n2*pow((n1+n2),2)/(pow(n1,3)+pow(n2,3))) - 6)*empiricalv2*empiricalv2*empiricalu2*empiricalu2))/2)*(Regions[u+2]-Regions[u+1]);
		area_u_v = area_u_v + ((area_u1_v+area_u2_v)/2)*(Regions[u+2]-Regions[u+1]);
	}
	return area_u_v;
}
