# Example 1: Defining the problem works

    Code
      Nop_ackley$print()
    Message
      
      -- Optimization problem --
      
      * Objective: ackley
      * Target: x (length 2)
      
      -- Fixed arguments --
      
      a
      b
      c
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

---

    Code
      print(Nop_ackley)
    Message
      
      -- Optimization problem --
      
      * Objective: ackley
      * Target: x (length 2)
      
      -- Fixed arguments --
      
      a
      b
      c
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

---

    Code
      Nop_ackley
    Message
      
      -- Optimization problem --
      
      * Objective: ackley
      * Target: x (length 2)
      
      -- Fixed arguments --
      
      a
      b
      c
      
      -- Optimizer functions --
      
      1. nlm
      2. stats::optim
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

# Example 1: Evaluation and optimization works

    Code
      Nop_ackley
    Message
      
      -- Optimization problem --
      
      * Objective: ackley
      * Target: x (length 2)
      
      -- Fixed arguments --
      
      a
      b
      c
      
      -- Optimizer functions --
      
      1. nlm
      2. stats::optim
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
      * Total runs: 20
      
      -- Optimization results --
      
      * Minimum parameter vector: 0, 0
      * Minimum function value: 0
    Output
      

# Example 2: Defining the problem works

    Code
      Nop_hmm$print()
    Message
      
      -- Optimization problem --
      
      * Objective: fHMM::ll_hmm
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      sdds
      states
      negative
      controls
      hierarchy
      check_controls
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

---

    Code
      print(Nop_hmm)
    Message
      
      -- Optimization problem --
      
      * Objective: fHMM::ll_hmm
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      sdds
      states
      negative
      controls
      hierarchy
      check_controls
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

---

    Code
      Nop_hmm
    Message
      
      -- Optimization problem --
      
      * Objective: fHMM::ll_hmm
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      sdds
      states
      negative
      controls
      hierarchy
      check_controls
      
      -- Optimizer functions --
      
      1. stats::nlm
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

# Example 2: Additional arguments can be modified and reset

    Code
      print(Nop_hmm)
    Message
      
      -- Optimization problem --
      
      * Objective: fHMM::ll_hmm
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
      sdds
      states
      negative
      controls
      hierarchy
      check_controls
      observations
      
      -- Optimizer functions --
      
      1. stats::nlm
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

---

    Code
      print(Nop_hmm)
    Message
      
      -- Optimization problem --
      
      * Objective: fHMM::ll_hmm
      * Target: parUncon (length 6)
      
      -- Fixed arguments --
      
        sdds
        states
        negative
        controls
        hierarchy
        check_controls
      ! observations
    Output
      
      Some arguments are currently modified.
    Message
      
      -- Optimizer functions --
      
      1. stats::nlm
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message
      -- Optimization runs --
      
    Output
      No results available.
      

