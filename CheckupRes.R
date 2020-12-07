checkupRes=function(res){
  res_cr = scale(res, center = TRUE, scale = TRUE)
  dev.off()
  layout(matrix(c(1,1,1,2:7), nrow=3, ncol=3, byrow=TRUE))
  plot(res, type = 'l')
  plot(acf(res, plot=F))
  plot(pacf(res, plot=F))
  plot(res[2:length(res)],res[1:length(res)-1])
  hist(res, freq = F)
  qqnorm(res, pch = 1, frame = FALSE)
  qqline(res, col = "red", lwd = 2)
  plot(res_cr)
  abline(h = c(-1.96,1.96), col = 'red',lty = 2)
}