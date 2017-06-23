fitDistrToGauss <- function(data, req.bins=10, fit.density=100,
init.amp=1, init.mean=1, init.sigma=1){

#calc histogram
hist.data=hist(data, breaks=req.bins, plot=FALSE)
bins <- hist.data$breaks
bins.length <- length(bins)
xHistPoints <- hist.data$mids
yHistValues <- hist.data$counts

#calc mean and st dev of Gauss
mean=mean(data)
stdev=sd(data)

#calc Gauss fit
model=nls(yHistValues ~ amp * exp(-(xHistPoints-mean)**2/(2 * sigma**2)),
 start=list(amp=init.amp, mean=init.mean, sigma=init.sigma))

#predict fitted model to a denser grid of x values
xFitPoints <- seq(bins[1],bins[bins.length],length=fit.density)
yFitPoints <- predict(model, newdata=data.frame(xHistPoints=xFitPoints))

result=list(x.values=xFitPoints,y.values=yFitPoints,
amp=coef(summary(model))["amp","Estimate"],
mean=coef(summary(model))["mean","Estimate"],
sigma=coef(summary(model))["sigma","Estimate"], model=model)

return(result)
}
