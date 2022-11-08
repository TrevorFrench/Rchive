global_var <- 'global'
local_var <- 'local'

global_var
# RETURNS: "global"
local_var
# RETURNS: "local"

my_function <- function() {
  global_var <<- 'na'
  local_var <- 'na'
  print(global_var)
  print(local_var)
}

my_function()
#RETURNS:
# "na"
# "na"

global_var
#RETURNS: "na"
local_var
#RETURNS: "local"

new_counter <- function() {
  i <- 0
  old_counter <- function() {
    # do something useful, then ...
    i <- i + 1
    i
  }
  old_counter()
  i
}

new_counter()