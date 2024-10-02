cap program drop dim_estimator
program define dim_estimator
  reg y i.treat
end

* Clean simulation -------------------------------------------------------------
cap program drop dgp_clean
program define dgp_clean
  syntax [, true_te(real 10.0)]

  // Set the number of observations
  local n = 100

  // Generate the x variable with random uniform values between 0 and 10
  qui clear
  qui set obs `n'
  generate x = runiform() * 10

  generate treat = 1 * (runiform() > 0.5)

  generate u = rnormal(0, 2)

  generate y = treat * `true_te' + x * 2 + u
end

cap program drop monte_carlo_simulation_clean
program define monte_carlo_simulation_clean, rclass
  dgp_clean, true_te(10)
  dim_estimator

  local tau_hat = _b[1.treat]
  return scalar tau_hat = `tau_hat'
end

simulate tau_hat = r(tau_hat), reps(1000) seed(20240917): monte_carlo_simulation_clean

histogram tau_hat, bin(20) normal frequency


* Broken simulation ------------------------------------------------------------
cap program drop dgp_broken
program define dgp_broken
  syntax [, true_te(real 10.0)]

  // Set the number of observations
  local n = 100

  // Generate the x variable with random uniform values between 0 and 10
  qui clear
  qui set obs `n'
  generate x = runiform() * 10

  generate treat = 1 * (runiform() > 0.5)

  generate u = rnormal(0, 2)

  generate y = treat * `true_te' + x * 2 + u
end

cap program drop monte_carlo_simulation_broken
program define monte_carlo_simulation_broken, rclass
  dgp_broken, true_te(10)
  dim_estimator

  local tau_hat = _b[1.treat]
  return scalar tau_hat = `tau_hat'
end

simulate tau_hat = r(tau_hat), reps(1000) seed(20240917): monte_carlo_simulation_broken

histogram tau_hat, bin(20) normal frequency

