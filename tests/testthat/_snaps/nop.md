# Nop object can be initialized

    Code
      ackley$print()
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Optimizer functions:
      No optimizer specified yet.
      Optimization results:
      No results saved yet.

---

    Code
      print(ackley)
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Optimizer functions:
      No optimizer specified yet.
      Optimization results:
      No results saved yet.

# Additional function arguments can be set

    Code
      print(hmm)
    Output
      Optimization problem:
      - Function: f_ll_hmm
      - Optimize over: theta (length 6) 
      - Additional arguments: data, N, neg 
      Optimizer functions:
      No optimizer specified yet.
      Optimization results:
      No results saved yet.

# Optimizer can be set

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Optimizer functions:
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
      Optimizer functions:
      - 1: nlm 
      - 2: stats::optim 
      Optimization results:
      No results saved yet.

# Optimization via random or fixed initialization works

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Optimizer functions:
      - 1: stats::nlm 
      - 2: stats::optim 
      Optimization results:
      - Optimizations runs: 20
      - Not comparable runs: 0
      - Failed runs: 0
      - Best parameter vector: 0 0
      - Best function value: 0 

