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

# Example 2: Defining the problem works

    Code
      Nop_hmm$print()
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      * sdds
      * states
      * negative
      * controls
      * hierarchy
      * check_controls

---

    Code
      print(Nop_hmm)
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      * sdds
      * states
      * negative
      * controls
      * hierarchy
      * check_controls

---

    Code
      Nop_hmm
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      * sdds
      * states
      * negative
      * controls
      * hierarchy
      * check_controls
      
      -- Optimizer functions --
      
      1. stats::nlm

# Example 2: Additional arguments can be modified and reset

    Code
      print(Nop_hmm)
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      * sdds
      * states
      * negative
      * controls
      * hierarchy
      * check_controls
      * observations
      
      -- Optimizer functions --
      
      1. stats::nlm

---

    Code
      print(Nop_hmm)
    Message
      
      -- Optimization problem --
      
      * Objective: f
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      * sdds
      * states
      * negative
      * controls
      * hierarchy
      * check_controls
      ! observations
      
      -- Optimizer functions --
      
      1. stats::nlm

