# Example 1: Defining the problem works

    Code
      Nop_ackley$print()
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ackley
      * Optimize over: x (length 2)
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message <cliMessage>
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

---

    Code
      print(Nop_ackley)
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ackley
      * Optimize over: x (length 2)
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message <cliMessage>
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

---

    Code
      Nop_ackley
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ackley
      * Optimize over: x (length 2)
      * True minimum value: 0
      * True minimum at: 0, 0
      
      -- Optimizer functions --
      
      1. nlm
      2. stats::optim
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

# Example 1: Evaluation and optimization works

    Code
      Nop_ackley
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ackley
      * Optimize over: x (length 2)
      * True minimum value: 0
      * True minimum at: 0, 0
      
      -- Optimizer functions --
      
      1. nlm
      2. stats::optim
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
      * Total runs: 20
      
      -- Optimization results --
      
      * Minimum parameter vector: 0, 0
      * Minimum function value: 0

# Example 2: Defining the problem works

    Code
      Nop_hmm$print()
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ll_hmm
      * Optimize over: theta (length 6)
      
      -- Additional function arguments --
      
      N
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message <cliMessage>
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

---

    Code
      print(Nop_hmm)
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ll_hmm
      * Optimize over: theta (length 6)
      
      -- Additional function arguments --
      
      N
      
      -- Optimizer functions --
      
    Output
      No optimizer specified.
      
    Message <cliMessage>
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

---

    Code
      Nop_hmm
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ll_hmm
      * Optimize over: theta (length 6)
      
      -- Additional function arguments --
      
      N
      
      -- Optimizer functions --
      
      1. stats::nlm
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

# Example 2: Additional arguments can be modified and reset

    Code
      print(Nop_hmm)
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ll_hmm
      * Optimize over: theta (length 6)
      
      -- Additional function arguments --
      
      N
      data
      
      -- Optimizer functions --
      
      1. stats::nlm
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

# Example 2: True value and parameter can be set

    Code
      print(Nop_hmm)
    Message <cliMessage>
      
      -- Optimization problem --
      
      * Function: ll_hmm
      * Optimize over: theta (length 6)
      * True maximum value: -139.545254
      * True maximum at: -2.3025851, -1.6094379, -2, 2, -0.6931472, 0
      
      -- Additional function arguments --
      
      N
      data
      
      -- Optimizer functions --
      
      1. stats::nlm
      
      -- Initial values --
      
    Output
      No initial values specified.
      
    Message <cliMessage>
      -- Optimization runs --
      
    Output
      No results.
      

