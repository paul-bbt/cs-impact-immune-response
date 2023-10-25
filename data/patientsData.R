P1 <- list(
  name = "Patient_1",
  pre_treatment_load = 73, # k/uL see paper, Table 2
  time_months = c(0,30,35,46), # Table 3
  time_days = c(0,30,35,46) * 31,
  sfcs_well = c(3,29,2525,9), # Table 3
  y0_0 = 7.6e-6, # Initial concentration of leukemia stem cells, Table 6
  n = 1.2, # Average number of T cell divisions, Table 6
  d_t = 0.001, # Anti-leukemia T cell death rate, Table 6
  s_t = 1.2e-6, # Anti-leukemia T cell supply rate, Table 6
  c_n = 1 # Decay rate of immune responsivity, Table 6
)

# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

P4 <- list(
  name = "Patient_4",
  pre_treatment_load = 23.1, # k/uL see paper
  time_months = c(0,6,9,18,24,32,34,42),
  time_days = c(0,6,9,18,24,32,34,42) * 31,
  sfcs_well = c(1,16.5,33,30,26,11,15,12),
  y0_0 = 2.4e-6,
  n = 1.2,
  d_t = 0.0022,
  s_t = 9e-7,
  c_n = 7
)

# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

P12 <- list(
  name = "Patient_12",
  pre_treatment_load = 116.8, # k/uL see paper
  time_months = c(0,2,5,9,13,15,24,30),
  time_days = c(0,2,5,9,13,15,24,30) * 31,
  sfcs_well = c(11,42,39,71,36.5,43,5,6),
  y0_0 = 1.2e-5,
  n = 1.17,
  d_t = 0.007,
  s_t = 3.08e-5,
  c_n = 0.8
)