
library(ggplot2)
path <- read.csv("C:/Users/k8hil/OneDrive/Desktop/ST 412/Week 8/galapagos.csv")
head(path)

# 1 Fit the Model
model <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = path)
summary(model)

# 2 Residuals vs. Fitted
path$residuals <- resid(model)
path$fitted <- fitted(model)

ggplot(path, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 3 Residuals vs. Island Area
ggplot(path, aes(x = Area, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Area", x = "Area of Island", y = "Residuals") +
  theme_minimal()

# 4 Leverage, Cook, and Standard
# Leverage Plot
lev <- hatvalues(model)

plot(lev, type = "h", main = "Leverage Values", ylab = "Leverage", xlab = "Observation Index")
abline(h = 2*mean(lev), col = "red", lty = 2)

# Cook's Distance
cook <- cooks.distance(model)

plot(cook, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4/(nrow(path) - length(model$coefficients)), col = "red", lty = 2)

# Standard Residuals
std_resid <- rstandard(model)

plot(std_resid, type = "h", main = "Standardized Residuals", ylab = "Standardized Residuals", xlab = "Observation Index")
abline(h = c(-2, 2), col = "blue", lty = 2)

# Potential influences
which(lev > 2*mean(lev))
which(cook > 4/(nrow(path) - length(model$coefficients)))
which(abs(std_resid) > 2)

# 5 Remove
cleaned_path <- path[-c(2, 10), ]

model_clean <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = cleaned_path)
summary(model_clean)

# Potential influences
which(lev > 2*mean(lev))
which(cook > 4/(nrow(path) - length(model$coefficients)))
which(abs(std_resid) > 2)






