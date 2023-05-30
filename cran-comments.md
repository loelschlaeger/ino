* If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

Reply: Unfortunately, there is no reference yet.

* Please write TRUE and FALSE instead of T and F.
'T' and 'F' instead of TRUE and FALSE:
  man/sim_hmm.Rd:
    sim_hmm(T, N, theta)
    
Reply: We use the input variable 'T', but this is not a logical but an integer.

* Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
Missing Rd-tags:
     Nop.Rd:  \value
     
Reply: Done.

* You have examples for unexported functions. Please either omit these examples or export these functions.
Examples for unexported function
  build_initial() in:
     build_initial.Rd
  filter_results() in:
     filter_results.Rd
  ino-package() in:
     is_count.Rd
  is_count() in:
     is_index_vector.Rd
  is_index_vector() in:
     is_name.Rd
  is_name() in:
     is_name_vector.Rd
  is_name_vector() in:
     is_number.Rd
  is_number() in:
     is_proportion.Rd
  is_proportion() in:
     is_time_limit.Rd
  is_time_limit() in:
     is_TRUE_FALSE.Rd
  sim_mnp() in:
     simplify_results.Rd
  simplify_results() in:
     standardize_argument.Rd
  standardize_argument() in:
     subset_argument.Rd
     
Reply: We omit examples from unexported functions.

* \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

Reply: We removed the \dontrun{}'s.

* You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)

Reply: To suppress print(), cat(), or message() outputs, our functions provide the 'verbose' argument. Setting 'verbose = FALSE' suppresses all of those. This can also be done globally via the 'options("ino_verbose" = FALSE)".


