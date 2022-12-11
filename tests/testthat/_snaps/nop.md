# function arguments can be set

    Code
      hmm
    Output
      Optimization problem:
       Function: f_ll_hmm
       Optimize over: theta (dimension 4)
       Additional arguments:
       - data 
       - N 
       - neg 
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

# optimizer details can be printed

    Code
      ackley$optimizer_details("all")
    Output
      1: A 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      
      2: B 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      
      3: C 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      
      4: stats::nlm 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      

---

    Code
      ackley$optimizer_details(c(3, 4, 1))
    Output
      1: A 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      
      3: C 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      
      4: stats::nlm 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      

---

    Code
      ackley$optimizer_details(c("stats::nlm", "B"))
    Output
      2: B 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      
      4: stats::nlm 
      List of 1
       $ :List of 5
        ..$ opt_fun  :function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), fscale = 1, 
          print.level = 0, ndigit = 12, gradtol = 1e-06, stepmax = max(1000 * 
              sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, iterlim = 100, 
          check.analyticals = TRUE)  
        ..$ opt_name : chr "stats::nlm"
        ..$ add      : list()
        ..$ arg_names:List of 4
        .. ..$ f: chr "f"
        .. ..$ p: chr "p"
        .. ..$ v: chr "minimum"
        .. ..$ z: chr "estimate"
        ..$ out_ign  : chr [1:2] "minimum" "estimate"
        ..- attr(*, "class")= chr "optimizer"
      

