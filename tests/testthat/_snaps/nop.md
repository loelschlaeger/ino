# Nop object can be printed

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

---

    Code
      print(ackley)
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

---

    Code
      ackley$print()
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

# Nop object with parameters can be initialized

    Code
      print(hmm)
    Output
      Optimization problem:
       Function: f_ll_hmm
       Optimize over: theta (dimension 6)
       Additional arguments: data 
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

---

    Code
      print(hmm)
    Output
      Optimization problem:
       Function: f_ll_hmm
       Optimize over: theta (dimension 6)
       Additional arguments: data, test_arg1, test_arg2 
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

# true value can be set

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
       True optimum value: 3
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

# true parameter can be set

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
       True optimum at: 0 0
       True optimum value: 0
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

# optimizer can be set

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       1: nlm
      Optimization results:
       No optimization records.

---

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       1: nlm
       2: stats::optim
      Optimization results:
       No optimization records.

# optimizer can be removed

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       1: A
       2: B
       3: C
       4: stats::nlm
      Optimization results:
       No optimization records.

---

    Code
      ackley2
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       No optimizer specified.
      Optimization results:
       No optimization records.

---

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       1: A
       2: C
       3: stats::nlm
      Optimization results:
       No optimization records.

---

    Code
      ackley
    Output
      Optimization problem:
       Function: f_ackley
       Optimize over: x (dimension 2)
      Numerical optimizer:
       1: C
      Optimization results:
       No optimization records.

