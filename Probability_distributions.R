#=====================================================
# Empirical and theoretical Distributions
#=====================================================
# Load libraries 
#=====================================================
library(tidyverse)
#install.packages("fitdistrplus")
library(fitdistrplus)
library(MASS)

data("Pima.tr")

# Continuous distributions
# Pima.tr$BMI
#=====================================================
# Explore Data: Histogram
#=====================================================
ggplot(Pima.tr, aes(x = bmi)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "skyblue", color = "white") +
  labs(title = "Values of BMI in Pima.tr", x = "BMI", y = "Probability Density") +
  theme_minimal()

#=====================================================
# Fit Candidate Distributions
#=====================================================
fit_norm  <- fitdist(Pima.tr$bmi, "norm")
fit_lnorm <- fitdist(Pima.tr$bmi, "lnorm")
fit_gamma <- fitdist(Pima.tr$bmi, "gamma")

# Inspect parameter estimates and plots
summary(fit_norm)
plot(fit_norm) 
summary(fit_lnorm)
plot(fit_lnorm) 
summary(fit_gamma)
plot(fit_gamma) 

#=====================================================
# Histogram with Fitted Curves
#=====================================================
Pima.tr %>%
  ggplot(aes(x = bmi)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "skyblue", color = "white", alpha = 0.6) +
  # Normal
  stat_function(fun = dnorm, 
                args = list(mean = fit_norm$estimate["mean"], 
                            sd   = fit_norm$estimate["sd"]), 
                aes(color = "Normal"), linewidth = 1.2) +
  
  # Lognormal
  stat_function(fun = dlnorm, 
                args = list(meanlog = fit_lnorm$estimate["meanlog"], 
                            sdlog   = fit_lnorm$estimate["sdlog"]), 
                aes(color = "Lognormal"), linewidth = 1.2) +
  
  # Gamma
  stat_function(fun = dgamma, 
                args = list(shape = fit_gamma$estimate["shape"], 
                            rate  = fit_gamma$estimate["rate"]), 
                aes(color = "Gamma"), linewidth = 1.2) +
  
  labs(x = "BMI", y = "Probability Density", 
       title = "Histogram of BMI with fitted distributions",
       color = "Distribution") +
  scale_color_manual(values = c("Normal" = "red", 
                                "Lognormal" = "darkgreen", 
                                "Gamma" = "blue")) +
  theme_minimal()

#=====================================================
# Goodness of Fit Statistics
#=====================================================
gof <- gofstat(list(fit_norm, fit_lnorm, fit_gamma))
print(gof)

#=====================================================
# QQ-Plots with Bands
#=====================================================
#install.packages("qqplotr")
library(qqplotr)

# Normal QQ
ggplot(Pima.tr, aes(sample = bmi)) +
  stat_qq_band(distribution = "norm", dparams = list(
    mean = fit_norm$estimate["mean"], 
    sd   = fit_norm$estimate["sd"])) +
  stat_qq_line(distribution = "norm", dparams = list(
    mean = fit_norm$estimate["mean"], 
    sd   = fit_norm$estimate["sd"])) +
  stat_qq_point(distribution = "norm", dparams = list(
    mean = fit_norm$estimate["mean"], 
    sd   = fit_norm$estimate["sd"])) +
  labs(title = "QQ-plot: Normal fit") +
  theme_minimal()

# Lognormal QQ
ggplot(Pima.tr, aes(sample = bmi)) +
  stat_qq_band(distribution = "lnorm", dparams = list(
    meanlog = fit_lnorm$estimate["meanlog"], 
    sdlog   = fit_lnorm$estimate["sdlog"])) +
  stat_qq_line(distribution = "lnorm", dparams = list(
    meanlog = fit_lnorm$estimate["meanlog"], 
    sdlog   = fit_lnorm$estimate["sdlog"])) +
  stat_qq_point(distribution = "lnorm", dparams = list(
    meanlog = fit_lnorm$estimate["meanlog"], 
    sdlog   = fit_lnorm$estimate["sdlog"])) +
  labs(title = "QQ-plot: Lognormal fit") +
  theme_minimal()

# Gamma QQ
ggplot(Pima.tr, aes(sample = bmi)) +
  stat_qq_band(distribution = "gamma", dparams = list(
    shape = fit_gamma$estimate["shape"], 
    rate  = fit_gamma$estimate["rate"])) +
  stat_qq_line(distribution = "gamma", dparams = list(
    shape = fit_gamma$estimate["shape"], 
    rate  = fit_gamma$estimate["rate"])) +
  stat_qq_point(distribution = "gamma", dparams = list(
    shape = fit_gamma$estimate["shape"], 
    rate  = fit_gamma$estimate["rate"])) +
  labs(title = "QQ-plot: Gamma fit") +
  theme_minimal()

# Discrete distributions
# Pima.tr$npreg
#=====================================================
# Explore Data: Histogram
#=====================================================
ggplot(Pima.tr, aes(x = npreg)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightgreen", color = "white") +
  labs(title = "Number of pregnancies in Pima.tr", x = "Number of pregnancies", y = "Probability Density") +
  theme_minimal()

#=====================================================
# Fit candidate distributions
#=====================================================
# Fit discrete distributions
fit_pois   <- fitdist(Pima.tr$npreg, "pois")
fit_nbinom <- fitdist(Pima.tr$npreg, "nbinom")
fit_geom   <- fitdist(Pima.tr$npreg, "geom")

# Check summaries and plots
summary(fit_pois)
plot(fit_pois)
summary(fit_nbinom)
plot(fit_nbinom)
summary(fit_geom)
plot(fit_geom)

# Build probability tables for plotting
pois_probs <- tibble(
  x = 0:max(Pima.tr$npreg),
  y = dpois(x, lambda = fit_pois$estimate["lambda"])
)

nbinom_probs <- tibble(
  x = 0:max(Pima.tr$npreg),
  y = dnbinom(x,
              size = fit_nbinom$estimate["size"],
              mu   = fit_nbinom$estimate["mu"])
)

geom_probs <- tibble(
  x = 0:max(Pima.tr$npreg),
  y = dgeom(x, prob = fit_geom$estimate["prob"])
)

# Plot histogram + fitted discrete distributions with legend
ggplot(Pima.tr, aes(x = npreg)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, fill = "lightgreen", color = "white", alpha = 0.6) +
  
  # Poisson
  geom_point(data = pois_probs, aes(x, y, color = "Poisson"), size = 2) +
  geom_line(data = pois_probs, aes(x, y, color = "Poisson"), linewidth = 0.3) +
  
  # Negative Binomial
  geom_point(data = nbinom_probs, aes(x, y, color = "Negative Binomial"), size = 2) +
  geom_line(data = nbinom_probs, aes(x, y, color = "Negative Binomial"), linewidth = 0.3) +
  
  # Geometric
  geom_point(data = geom_probs, aes(x, y, color = "Geometric"), size = 2) +
  geom_line(data = geom_probs, aes(x, y, color = "Geometric"), linewidth = 0.3) +
  
  labs(title = "Number of pregnancies with fitted discrete distributions",
       x = "Number of Pregnancies", y = "Probability density",
       color = "Distribution") +
  scale_color_manual(values = c("Poisson" = "darkgreen",
                                "Negative Binomial" = "red",
                                "Geometric" = "purple")) +
  theme_minimal()

#=====================================================
# Goodness of fit statistics (DISCRETE only)
#=====================================================
gof <- gofstat(list(fit_pois, fit_nbinom, fit_geom))
print(gof)


#=====================================================
# QQ-plots with confidence bands (DISCRETE distributions)
#=====================================================

# Poisson QQ
ggplot(Pima.tr, aes(sample = npreg)) +
  stat_qq_band(distribution = "pois",
               dparams = list(lambda = fit_pois$estimate["lambda"])) +
  stat_qq_line(distribution = "pois",
               dparams = list(lambda = fit_pois$estimate["lambda"])) +
  stat_qq_point(distribution = "pois",
                dparams = list(lambda = fit_pois$estimate["lambda"])) +
  labs(title = "QQ-plot: Poisson fit (npreg)") +
  theme_minimal()

# Negative Binomial QQ
ggplot(Pima.tr, aes(sample = npreg)) +
  stat_qq_band(distribution = "nbinom",
               dparams = list(size = fit_nbinom$estimate["size"],
                              mu   = fit_nbinom$estimate["mu"])) +
  stat_qq_line(distribution = "nbinom",
               dparams = list(size = fit_nbinom$estimate["size"],
                              mu   = fit_nbinom$estimate["mu"])) +
  stat_qq_point(distribution = "nbinom",
                dparams = list(size = fit_nbinom$estimate["size"],
                               mu   = fit_nbinom$estimate["mu"])) +
  labs(title = "QQ-plot: Negative Binomial fit (npreg)") +
  theme_minimal()

# Geometric QQ
ggplot(Pima.tr, aes(sample = npreg)) +
  stat_qq_band(distribution = "geom",
               dparams = list(prob = fit_geom$estimate["prob"])) +
  stat_qq_line(distribution = "geom",
               dparams = list(prob = fit_geom$estimate["prob"])) +
  stat_qq_point(distribution = "geom",
                dparams = list(prob = fit_geom$estimate["prob"])) +
  labs(title = "QQ-plot: Geometric fit (npreg)") +
  theme_minimal()
