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
      No optimization results saved yet.

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
      No optimization results saved yet.

# optimizer can be removed

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      - 1: A 
      - 2: B 
      - 3: C 
      - 4: stats::nlm 
      Optimization results:
      No optimization results saved yet.

---

    Code
      ackley2
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
    Warning <rlang_warning>
      ! No optimizer selected.
      ! No active optimizer specified.
      ! No optimizer selected.
      ! No active optimizer specified.
      ! No optimizer selected.
      ! No active optimizer specified.
      ! No optimizer selected.
      ! No active optimizer specified.
    Output
      - 1: A has been removed. 
    Warning <rlang_warning>
      ! No optimizer selected.
      ! No active optimizer specified.
      ! No optimizer selected.
      ! No active optimizer specified.
    Output
      - 2: B has been removed. 
    Warning <rlang_warning>
      ! No optimizer selected.
      ! No active optimizer specified.
      ! No optimizer selected.
      ! No active optimizer specified.
    Output
      - 3: C has been removed. 
    Warning <rlang_warning>
      ! No optimizer selected.
      ! No active optimizer specified.
      ! No optimizer selected.
      ! No active optimizer specified.
    Output
      - 4: stats::nlm has been removed. 
      Optimization results:
      No optimization results saved yet.

---

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      - 1: A 
      - 2: B has been removed. 
      - 3: C 
      - 4: stats::nlm 
      Optimization results:
      No optimization results saved yet.

---

    Code
      ackley
    Output
      Optimization problem:
      - Function: f_ackley
      - Optimize over: x (length 2) 
      Numerical optimizer:
      - 1: A has been removed. 
      - 2: B has been removed. 
      - 3: C 
      - 4: stats::nlm has been removed. 
      Optimization results:
      No optimization results saved yet.

# function can be optimized

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
      - Optimization runs: 10
      - Best parameter: 0 0
      - Best value: 0 

