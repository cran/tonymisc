lagger <- function(data, by.var, lag.vars, num.lags, direction, freq = "daily") {
  if(class(data$date) != "Date"){
    cat("date is not in Date format. No lagging operations performed.")
    retdat = NULL
  }
  if(class(data$date) == "Date"){
    data$year = format(data$date, "%Y")
    data$month = format(data$date, "%m")
    data$quarter = as.factor(quarters(data$date))
    buy.var = "ordermaker"
    data[, buy.var] <- as.numeric(as.factor(data[,by.var]))
  
  if (freq == "daily") {
    data$date.num <- as.numeric(data$date)
    bad.obs <- 4
  }
  if (freq == "monthly") {
    data$date.num <- as.numeric(data$year)*100 + (as.numeric(data$month)*100/12)
    bad.obs <- 100/11
  }
  if (freq == "quarterly") {
    data$date.num <- as.numeric(data$year)*10 + (as.numeric(data$quarter)*10/4)
    bad.obs <- 3
  }
  if (direction == "lag") {
    data <- data[order(data[, buy.var], data$date.num), ]
  }
  if (direction == "lead") {
    data <- data[order(data[, buy.var], data$date.num, decreasing = TRUE), ]
  }
  lags <- c(buy.var, "date.num", lag.vars)
  nums <- c(rep(max(num.lags), 2), num.lags)
  g <- data.frame(lags, nums, stringsAsFactors = FALSE)
  for (v in 1:nrow(g)) {
    var <- g[v, 1]
    print(var)
    for (n in 1:g[v, 2]) {
      data[, paste(var, paste(direction, n, sep = ""), sep = ".")] <- c(rep(NA, times = n), data[1:(nrow(data) - n), var])
      if (var != buy.var & var != "date.num") {
        data[data[, paste(buy.var, paste(direction, n, sep = ""), sep = ".")] != data[, buy.var] | is.na(data[, paste(buy.var, paste(direction, n, sep = ""), sep = ".")]) | abs(data[, "date.num"] - data[, paste("date.num", paste(direction, n, sep = ""), sep = ".")]) > n*bad.obs, paste(var, paste(direction, n, sep = ""), sep = ".")] <- NA
      }
      assign("data", data)
    }
  }
    retdat = data[, names(data)[!grepl("date.num", names(data)) & !grepl("ordermaker", names(data))]] 
  }
  return(retdat)
}
