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

