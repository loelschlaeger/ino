# Nop object can be initialized

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

---

    Code
      print(ackley)
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

---

    Code
      ackley$print()
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

# Nop object with parameters can be initialized

    Code
      print(hmm)
    Output
      Optimization problem:
       Function: f_ll_hmm
       Optimize over: theta (length 6)
       Additional arguments: data 
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

---

    Code
      print(hmm)
    Output
      Optimization problem:
       Function: f_ll_hmm
       Optimize over: theta (length 6)
       Additional arguments: data, test_arg1, test_arg2 
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

# true value can be set

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
       True optimum value: 3
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

# true parameter can be set

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
       True optimum at: 0 0
       True optimum value: 0
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

# optimizer can be set

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       1: nlm
      Optimization results:
       No optimization results saved.

---

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       1: nlm
       2: stats::optim
      Optimization results:
       No optimization results saved.

# optimizer can be removed

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       1: A
       2: B
       3: C
       4: stats::nlm
      Optimization results:
       No optimization results saved.

---

    Code
      ackley2
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization results saved.

---

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       1: A
       2: C
       3: stats::nlm
      Optimization results:
       No optimization results saved.

---

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       1: C
      Optimization results:
       No optimization results saved.

# function can be optimized

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (length 2)
      Numerical optimizer:
       1: stats::nlm
       2: stats::optim
      Optimization results:
       Optimization runs: 10
       Best parameter: 0 0
       Best value: 0

