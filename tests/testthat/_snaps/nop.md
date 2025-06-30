# Example 0: Defining the problem works

    Code
      Nop_pol
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: x (length 1)

# Example 1: Minimization works

    Code
      Nop_ackley
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: x (length 2)
      
      -- Fixed arguments --
      
      * a
      * b
      * c
      
      -- Optimizer functions --
      
      1. nlm
      2. stats::optim
      
      -- Initial values --
      
      * 9x grid
      
      -- Optimization results --
      
      * Total: 22
      * error: 100% FALSE
      * .optimization_label: 46% unlabeled_2, 28% unlabeled_5, 10% unlabeled_1, 10%
        unlabeled_3, 10% unlabeled_4
      * .optimizer_label: 50% nlm, 50% stats::optim
      * .direction: 100% min
      * .original: 100% TRUE

