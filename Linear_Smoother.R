### R script which examines different data smoothing approaches. Run this in an R environment such as Rstudio.
### author: "Pierre Winter"

# Here we present 3 smoothing algorithms which are applied to 2 datasets
# One dataset contains traffic data in Switzerland and the other dataset consists of solar radiation data.
# We find that a simple running mean algorithm (with a window size of 3 years) does not provide a smooth line but captures the general trend of the data.
# The Gaussian algorithm does a better job at creating a curved line which follows most of the data nicely.
# The LOESS smoother (optimized for span and polynomial degree) gives the best estimate and creates a smooth line that closely follows all data points.

x <- 2006:2016
y <- c(112,109,91,98,139,150,168,149,149,135,140)
df <- data.frame(x,y)
plot(x, y, 
     main = "Days with traffic jams at Northern Gotthard Tunnel", 
     xlab = "Year", 
     ylab = "Days of Traffic", 
     pch = 18,     # Optional: this changes the symbol of the points.
     col = "blue", # Optional: color of the points.
     lwd = 2,      # Optional: line width of the points.
     cex = 2       # Optional: this specifies how many times larger 
                   # (or smaller) than the usual size of 1 we want 
                   # our points. To make the symbols smaller,
                   # a cex-value smaller than 1 is employed. 
     )

fit.sol <- ksmooth(x, y, bandwidth = 3, x.points = x)
x.fit <- fit.sol$x
y.fit <- fit.sol$y

fitn.sol <- ksmooth(x, y, bandwidth = 3, x.points = x, kernel = "normal")
x.fitn <- fitn.sol$x
y.fitn <- fitn.sol$y

loess.sol <- loess(y ~ x, data = df, span = 0.85, degree = 2)
smoothed <- predict(loess.sol)

plot(x, y, 
     main = "Days with Traffic Jams at Northern Gotthard Tunnel", 
     xlab = "Year", 
     ylab = "Days of Traffic", 
     pch = 20,
     col = "blue",
     lwd = 2,
     cex = 2,
     ylim=c(70,170)
     )
lines(x.fit, y.fit, 
      type = "l",   # We can specify the type of the plotted line.
      col = "red", 
      lwd = 2)
points(x.fit, y.fit, 
       col = "red", 
       lwd = 2, 
       cex = 1)

lines(x.fitn, y.fitn, 
      type = "l",
      col = "green", 
      lwd = 2)
points(x.fitn, y.fitn, 
       col = "green", 
       lwd = 2, 
       cex = 1)

lines(x, smoothed, 
      type = "l",
      col = "black", 
      lwd = 2)
points(x, smoothed, 
       col = "black", 
       lwd = 2, 
       cex = 1)

legend("bottomright",  # Alternatively, x and y coordinates can be specified explicitly.
       legend = c("Data Points", "Running Mean", "Gaussian", "LOESS"), 
       col = c("blue", "red", "green", "black"), 
       lwd = 3)


### Solar Radiation Analysis


load("R_datasets\\solar.radiation.rda")
soldata = sol.rad
head(soldata)

#Set first and last values of 9999 to their neighbor values
soldata[1,]$rad = soldata[2,]$rad
soldata$rad[dim(soldata)[1]] = soldata$rad[dim(soldata)[1]-1]

x <- soldata$jahr
y <- soldata$rad

plot(x, y, 
     pch = 18,
     col = "blue",
     lwd = 2,
     cex = 2
     )

fit.sol <- ksmooth(x, y, bandwidth = 10, x.points = x, kernel="box")
x.fit <- fit.sol$x
y.fit <- fit.sol$y


fitn.sol <- ksmooth(x, y, bandwidth = 10, x.points = x, kernel = "normal")
x.fitn <- fitn.sol$x
y.fitn <- fitn.sol$y

loess.sol <- loess(y ~ x, data = soldata, span = 0.65, degree = 2)
smoothed <- predict(loess.sol)

plot(x, y, 
     main = "Yearly Solar Radiation Intensity", 
     xlab = "Year", 
     ylab = "Solar Radiation Intensity", 
     pch = 18,
     col = "blue",
     lwd = 2,
     cex = 2
     )
lines(x.fit, y.fit, 
      type = "l",
      col = "red", 
      lwd = 2)
points(x.fit, y.fit, 
       col = "red", 
       lwd = 2, 
       cex = 1)

lines(x.fitn, y.fitn, 
      type = "l",
      col = "green", 
      lwd = 2)
points(x.fitn, y.fitn, 
       col = "green", 
       lwd = 2, 
       cex = 1)

lines(x, smoothed, 
      type = "l",
      col = "black",
      lwd = 2)
points(x, smoothed, 
       col = "black", 
       lwd = 2, 
       cex = 1)

legend("topright",
       legend = c("Data Points", "Running Mean", "Gaussian", "LOESS"), 
       col = c("blue", "red", "green", "black"), 
       lwd = 3)
