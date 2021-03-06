data <- data.frame(read.csv("dataEN.csv"));


# 建立多元线性回�?
lm <- lm(price~.,data=data);
# 去除年系�?
lm <- update(lm,price~.-year);
# 逐步回归
step(lm);
# 新回归模�?
lm.new <- step(lm);
# 去掉显著性不明显的系�?
lm.price <- update(lm.new,price~.-graduate);

# Call:
# lm(formula = price ~ CPI + RPI + population + estateSum + completionArea, 
#     data = data)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -552.13 -318.41  -94.15  168.37  939.06 

# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -3.502e+04  7.469e+03  -4.688 0.000857 ***
# CPI            -3.910e+01  1.911e+01  -2.046 0.067946 .  
# RPI             7.248e+01  1.501e+01   4.829 0.000693 ***
# population      2.017e+01  2.373e+00   8.499 6.91e-06 ***
# estateSum       3.663e+00  9.741e-01   3.760 0.003723 ** 
# completionArea -1.190e+00  2.616e-01  -4.551 0.001057 ** 
# ---
# Signif. codes:  0 â�??***â�?? 0.001 â�??**â�?? 0.01 â�??*â�?? 0.05 â�??.â�?? 0.1 â�?? â�?? 1

# Residual standard error: 551.5 on 10 degrees of freedom
# Multiple R-squared:  0.9967,	Adjusted R-squared:  0.995 
# F-statistic: 599.4 on 5 and 10 DF,  p-value: 4.752e-12

# 单一变量预测
# 人口
year <- c(data$year);
population <- c(data$population);
lm.people <- lm(population~year);
plot(year,population);
lines(year,fitted(lm.people));
predict(lm.people,data.frame(year=2014))

#cpi
cpi <- c(data$CPI)
plot(year,cpi);
lm.cpi <- lm(dpi~year^2);
lines(year,fitted(lm.cpi));
lm.cpi <- update(lm.cpi,cpi~.+I(year^2));
plot(year,cpi);
lines(year,fitted(lm.cpi));

#rpi
rpi <- c(data$RPI);
plot(year,rpi)
lm.rpi <- lm(rpi~year)
lm.rpi <- update(lm.rpi,rpi~.+I(year^2))
lines(year,fitted(lm.rpi))

#estateSum
sum <- c(data$estateSum)
plot(year,sum)
lm.sum <- glm(sum~year)
lines(year,fitted(lm.sum))
m.sum <- update(lm.sum,sum~.+I(year^2))
plot(year,sum)
lines(year,fitted(lm.sum))

#area
area <- c(5068,3828,2970,2776,2913,2838,2698)
areaYear <- c(2007,2008,2009,2010,2011,2012,2013)
lm.area <- lm(area~areaYear)
lm.area <- update(lm.area,.~.+I(areaYear^2))
plot(areaYear,area)
lines(areaYear,fitted(lm.area))

#repeat
area <- data$completionArea
lm.price <- lm(price~cpi+rpi+population+sum+area)
summary(lm.price)

#simple predict
year <- c(data$year)
price <- c(data$price)
plot(year,price)
lm.round <- lm(price~year)
lm.round <- update(lm.round,.~.+I(year^2))
lines(year,fitted(lm.round))
predict(lm.round,data.frame(year=2014))
lm.round$coeff

