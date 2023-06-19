# Nop object can be printed

    Code
      print(ackley)
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      No optimizer specified yet.
      Optimization results:
      No results saved yet.

---

    Code
      ackley$print()
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      No optimizer specified yet.
      Optimization results:
      No results saved yet.

# Parameters for Nop object can be set

    Code
      print(hmm)
    Output
      Optimization problem:
      - Function: f_ll_hmm
      - Optimize over: theta (length 6) 
      - Additional arguments: data 
      Numerical optimizer:
      No optimizer specified yet.
      Optimization results:
      No results saved yet.

# optimizer can be set

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      - 1: nlm 
      Optimization results:
      No results saved yet.

---

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      - 1: nlm 
      - 2: stats::optim 
      Optimization results:
      No results saved yet.

# ackley function can be optimized

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      - 1: stats::nlm 
      - 2: stats::optim 
      Optimization results:
      - Total runs (comparable): 13 (13)
      - Best parameter: 0 0
      - Best value: 0 

