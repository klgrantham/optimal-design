# Generate variance results for plotting
#
# Kelsey Grantham (kelsey.grantham@monash.edu)

source('optimal_design.R')

# Create results directory if no such directory exists
if (!dir.exists('results')) {
  dir.create('results')
}

# Variance results for several decay rates (Figures 3 and 4)
optimal_T(r=0.95, rho0=0.036, M=2000, N=40)
optimal_T(r=0.90, rho0=0.036, M=2000, N=40) 
optimal_T(r=0.77, rho0=0.036, M=2000, N=40)
optimal_T(r=0.50, rho0=0.036, M=2000, N=40)

# Variance results with cost constraints (Figure 5)
optimal_N_T_fixedM(r=0.77, rho0=0.036, M=2000, maxN=40, B=2500000, c=2500, s=50, x=250)

# Variance results for smaller cluster size (Figure 6)
optimal_T(r=0.77, rho0=0.036, M=200, N=40)

# Variance results for several base correlation values
optimal_T(r=0.77, rho0=0.01, M=2000, N=40)
optimal_T(r=0.77, rho0=0.1, M=2000, N=40)
optimal_T(r=0.77, rho0=0.2, M=2000, N=40)