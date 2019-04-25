Cram.test <-
  function(Data1, Data2, P.Value=T, GridPoints=50){
    n1 = length(Data1)
    n2 = length(Data2)
    Data = c(Data1, Data2)
    n=length(Data)
    ts = numeric(n)
    SortedData =c(sort(Data), max(Data))
    for (i in 1:n){
      ts[i] = ((sum(Data1<=SortedData[i])/n1 - sum(Data2<=SortedData[i])/n2)^2) * (SortedData[i+1]-SortedData[i])
    }
    TestStatistic = ((n1*n2)/(n1+n2))*sum(ts)
    if (P.Value == F){
      return(list(Statistic = TestStatistic))
    }else{
      if (GridPoints == "Data"){
        Regions = sort(Data)
        mean = TestStatExpectation(Data, Regions)
        variance = TestStatVariance(n1, n2, Data, Regions)
        moment3 = TestStatMoment3(n1, n2, Data, Regions)
        skewness = (moment3 - 3*mean*variance - mean^3)/(variance^(3/2))
        xi= uniroot(Calc.Xi, skewness = skewness, interval=c(-10,1/3))$root
        sigma = uniroot(Calc.Sigma, xi=xi, variance=variance, interval=c(0,100))$root
        mu = uniroot(Calc.Mu, xi=xi, sigma=sigma, mean=mean, interval=c(-30,30))$root
      }else{
        Regions = seq(min(Data), max(Data), length=GridPoints)
        mean = TestStatExpectation(Data, Regions)
        variance = TestStatVariance(n1, n2, Data, Regions)
        moment3 = TestStatMoment3(n1, n2, Data, Regions)
        skewness = (moment3 - 3*mean*variance - mean^3)/(variance^(3/2))
        xi= uniroot(Calc.Xi, skewness = skewness, interval=c(-10,1/3))$root
        sigma = uniroot(Calc.Sigma, xi=xi, variance=variance, interval=c(0,100))$root
        mu = uniroot(Calc.Mu, xi=xi, sigma=sigma, mean=mean, interval=c(-30,30))$root
      }
      p.value = 1-as.numeric(pgpd(TestStatistic, xi = xi, mu = mu, beta = sigma))
      return(list(Statistic = TestStatistic, P.Value = p.value))
    }
  }

