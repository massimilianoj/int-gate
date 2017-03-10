library(tidyverse)

# "sin" -> "\\sin"
latexify = function(base, asis = FALSE) {
  if (asis) return(base)
  paste0("\\", base)
}

# "sin" -> "`\\sin`" 
put_codechunk = function(base, ...) paste0("`", latexify(base, ...), "`")

# "sin" -> "$\\sin$"
put_result = function(base, ...) paste0("$", latexify(base, ...), "$")

# Create a cheatsheet table
math_table = function(cmd, description = NULL) {
  # cmd = c("sin", "cos", "log") 
  # description = c("sine", "cosine", "logarithm")
  #
  # => A tibble with additional columns like 
  #      latex = c("\\sin", "\\cos", "\\log") 
  #    will be created 
  # If description is NULL, cmd column is copied. 
  
  if (is.null(description)) description = cmd 
  tbl = tibble(description, cmd) %>% 
    mutate(latex = put_codechunk(cmd), 
           result = put_result(cmd)) %>% 
    select(-cmd)
}

two_column_layout = function(s) {
  cat("<div class=\"twocol\">")
  cat("<div class=\"left\">")
  cat("**LaTeX CODE**\n")
  cat("```", "$$", s, "$$", "```", sep = "\n")
  cat("</div><!-- left -->")
  cat("<div class=\"right\">")
  cat("**RESULT**\n")
  cat("$$", s, "$$", sep = "\n")
  cat("</div><!-- right -->")
  cat("</div><!-- twocol -->")
}