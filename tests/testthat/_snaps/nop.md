# Nop object can be printed

    Code
      print(ackley)
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizers:
      No optimizer specified.
      Optimization results:

---

    Code
      ackley$print()
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizers:
      No optimizer specified.
      Optimization results:

# Parameters for Nop object can be set

    Code
      print(hmm)
    Output
      Optimization problem:
      - Function: f_ll_hmm
      - Optimize over: theta (length 6) 
      - Further arguments: data 
      Numerical optimizers:
      No optimizer specified.
      Optimization results:

# Optimizer can be set

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizers:
      - 1: nlm 
      Optimization results:

---

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizers:
      - 1: nlm 
      - 2: stats::optim 
      Optimization results:

# Ackley function can be optimized

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizers:
      - 1: stats::nlm 
      - 2: stats::optim 
      Optimization results:

