rm(list = ls())
library(magrittr)

# load data ----
# set constant ----
# monte carlo simulation to get nice sample size identifying match effect
set.seed(1)
num_time <-
  50
ar1_parameter_unemployed <-
  0.2
ar1_parameter_vacancy <-
  0.2
ar1_parameter_efficiency <-
  0.2
hire_parameter <-
  0.001
scale_parameter <-
  1e4
maxA <-
  300  # Maximum matching efficiency in grid (normalized at median vacancies to 100)
minA <-
  10   # Minimum matching efficiency in grid (normalized at median vacancies to 100)

time <-
  seq(1:num_time)
unemployed <-
  seq(1:num_time)
vacancy <-
  seq(1:num_time)
efficiency <-
  seq(1:num_time)
hire_list <-
  seq(1:num_time)

# data generation ----
unemployed <- 
  exp(
    arima.sim(
      list(
        order = c(1,0,0), # ar 1
        ar = ar1_parameter_unemployed
      ), 
      n = num_time
    )
  ) * scale_parameter
plot(unemployed)

vacancy <- 
  exp(
    arima.sim(
      list(
        order = c(1,0,0), # ar 1
        ar = ar1_parameter_vacancy
      ), 
      n = num_time
    )
  ) * scale_parameter
plot(vacancy)

efficiency <- 
  exp(
    arima.sim(
      list(
        order = c(1,0,0), # ar 1
        ar = ar1_parameter_efficiency
      ), 
      n = num_time
    )
  )
plot(efficiency)
normalized_efficiency <-
  (efficiency/
  efficiency[1]) * 10

data <-
  cbind(
    time,
    unemployed,
    vacancy,
    efficiency,
    normalized_efficiency
  ) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(
    hire =
      hire_parameter * 
      normalized_efficiency *
      unemployed +
      hire_parameter *
      vacancy
  ) %>% 
  dplyr::mutate(
    overhire_check =
      (unemployed <= hire) |
      (vacancy <= hire)
  )

# estimate ----

vstar <- 
  quantile(
    data$vacancy, 
    probs = 0.50
    )

# Find the index of the closest value to vstar in v
point_vstar <- 
  which.min(abs(data$vacancy - vstar))
# Get corresponding values of m and S
Hstar <- 
  data$hire[point_vstar]
Ustar <- 
  data$unemployed[point_vstar]
# Value of matching efficiency at normalization point
Astar <- 
  100

# Define the support for lambda and theta
supportlambda <- 
  data$unemployed / Ustar
supportsize_theta <-
  100  # Size of the support for theta
kernel_sd <-
  0.1  # Standard deviation of the kernel (as a percentage of the range)
supporttheta <- 
  seq(
    minA / Astar, 
    maxA / Astar, 
    length.out = supportsize_theta
    )

# Calculate support for A
supportA <- 
  Astar * supporttheta

# Calculate supportU
supportU <- 
  Ustar * supportlambda #= data$unemployed

# For kernel
hsvv <- 
  kernel_sd * diff(range(data$vacancy))
hsu <-
  kernel_sd *
  max(c(diff(range(data$unemployed)), 1))





# FAS1 = Matrix F(A|U) = contains the conditional distribution of matching efficiency A across S
# each column is a distribution conditional on the point in S defined by a value of S
# Initiate F(A|U) as a zero matrix
FAS1 <- 
  matrix(
    0, 
    nrow = length(supporttheta), 
    ncol = length(supportlambda)
    )  
for (il in 1:length(supportlambda)) {  # loop over the handle lambda over the S values
  lambda <- 
    supportlambda[il]
  for (it in 1:length(supporttheta)) {  # loop over handle theta corresponding to A
    theta <- 
      supporttheta[it]
    y <- 
      as.numeric(
        data$hire <= theta * lambda * Hstar
        )  
    # This does both Step 2 and 3
    # find G(H|U(i),v(j)) -- for each pair (U',v') we want to estimate the distribution over matches
    # below some value H. To do this, we average observed proportions
    # of H below this value across values of (U,v) that are close to
    # (U',v') -- weighted by a kernel to put more weight on close
    # values.
    kernel_input <-
      cbind(
        ((data$vacancy - theta * lambda * vstar) / hsvv),
        ((data$unemployed - lambda * Ustar) / hsu)
        )
    kernel_output <-
      mvtnorm::dmvnorm(kernel_input)
    FAS1[it, il] <- 
      sum(y * kernel_output) / 
      sum(kernel_output)
  }
}

# Predict efficiency A at each (H,U,V)
# (since (U,V,A) map one-to-one into H and we can just invert it)
efficiency_implied <- 
  numeric(length(data$hire))  # aggregate matching efficiency
for (i in 1:length(data$hire)) {
  y <- 
    as.numeric(
      data$hire <= data$hire[i]
      )
  # find G_H|u(i),v(i) at point H(i)
  kernel_input <-
    cbind(
      ((data$vacancy - data$vacancy[i]) / hsvv),
      ((data$unemployed - data$unemployed[i]) / hsu)
      )
  kernel_output <-
    mvtnorm::dmvnorm(kernel_input)
  GHsv <- 
    sum(y * kernel_output) / 
    sum(kernel_output)  
  unemployed_index <- 
    which.min(abs(supportU - data$unemployed[i]))
  efficiency_index <- 
    which.min(abs(FAS1[, unemployed_index] - GHsv))
  efficiency_implied[i] <- 
    supportA[efficiency_index]
}

A <- 
  efficiency_implied  # aggregate matching efficiency
plot(ts(A))
true_efficiency_normalized <-
  ts((data$normalized_efficiency/
        data$normalized_efficiency[point_vstar]) * 100
     )

plot(x=A, type="l", col=1, xlab = "Time", ylab="Normalized A")
lines(x=true_efficiency_normalized, col=2)
legend(
  "topleft", 
  legend=c("LP", "true"), 
  lty=1,
  col=1:2
  )
data <-
  cbind(
    data,
    A
    )%>% 
  dplyr::mutate(
    efficiency_unemployed =
      unemployed*A
  )

res <-
  lm(hire ~ vacancy + efficiency_unemployed,
   data = data)
elasticity_vacancy <-
  res$coefficients["efficiency_unemployed"] *
  (data$efficiency_unemployed/data$hire)
plot(ts(elasticity_vacancy))
elasticity_unemployed <-
  res$coefficients["vacancy"] *
  (data$vacancy/data$hire)
plot(ts(elasticity_unemployed))


