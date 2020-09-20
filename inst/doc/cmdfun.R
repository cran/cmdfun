## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "/bin", sep = ":"))

## -----------------------------------------------------------------------------
library(magrittr)
library(cmdfun)

## -----------------------------------------------------------------------------
get_all <- function(arg1, arg2, ...){
  cmd_args_all()
}

get_named <- function(arg1, arg2, ...){
  cmd_args_named()
}

get_dots <- function(arg1, arg2, ...){
  cmd_args_dots()
}

## -----------------------------------------------------------------------------
# cmd_args_all() gets all keword arguments and arguments passed to "..."
(argsListAll <- get_all("input", NA, bool = TRUE, vals = c(1,2,3)))

## -----------------------------------------------------------------------------
# cmd_args_named() gets all keword arguments, excluding arguments passed to "..."
(argsListNamed <- get_named("input", NA, bool = TRUE, vals = c(1,2,3)))

## -----------------------------------------------------------------------------
# cmd_args_dots() gets all arguments passed to "...", excluding keyword arguments
(argsListDots <- get_dots("input", NA, bool = TRUE, vals = c(1,2,3)))

## -----------------------------------------------------------------------------
# Note that `arg2` is dropped, and `bool` is converted to ""
cmd_list_interp(argsListAll) 

## -----------------------------------------------------------------------------
(flagList <- cmd_list_interp(argsListAll, c("bool" = "b")))

## -----------------------------------------------------------------------------
cmd_list_to_flags(flagList)

## -----------------------------------------------------------------------------
library(magrittr)

shell_ls <- function(dir = ".", ...){
  # grab arguments passed to "..." in a list
  flags <- cmd_args_dots() %>% 
    # prepare list for conversion to vector
    cmd_list_interp() %>% 
    # Convert the list to a flag vector
    cmd_list_to_flags()
  
  # Run ls shell command
  system2("ls", c(flags, dir), stdout = TRUE)
}

## -----------------------------------------------------------------------------
# list all .md files in ../
shell_ls("../*.md")

## -----------------------------------------------------------------------------
shell_ls("../*.md", l = TRUE)

## -----------------------------------------------------------------------------

shell_ls_alias <- function(dir = ".", ...){
  
  # Named vector acts as lookup table
  # name = function argument
  # value = flag name
  names_arg_to_flag <- c("long" = "l")
  
  flags <- cmd_args_dots() %>% 
    # Use lookup table to manage renames
    cmd_list_interp(names_arg_to_flag) %>% 
    cmd_list_to_flags()
  
  system2("ls", c(flags, dir), stdout = TRUE)
}

## -----------------------------------------------------------------------------
shell_ls_alias("../*.md", long = TRUE)

## -----------------------------------------------------------------------------
shell_cut <- function(text, ...){

  names_arg_to_flag <- c("sep" = "d",
                         "fields" = "f")
    
	flags <- cmd_args_dots() %>%
		cmd_list_interp(names_arg_to_flag) %>% 
	  cmd_list_to_flags()

	system2("cut", flags, stdout = T, input = text)
}

## -----------------------------------------------------------------------------
shell_cut("hello_world", fields = 2, sep = "_") 

## -----------------------------------------------------------------------------
# Note that the flag name values are accepted even when using a lookup table
shell_cut("hello_world_hello", f = c(1,3), d = "_") 

## ---- echo = FALSE------------------------------------------------------------
# This handles build system not having meme installed
# creates empty files representing real install
search_meme_path <- cmd_path_search(default_path = "~/meme/bin", utils = c("ame", "dreme"))
meme_is_installed <- cmd_install_is_valid(search_meme_path)

dummy_meme <- FALSE
if (!meme_is_installed()) {
  meme_loc <- "~/meme/bin"
  dir.create(meme_loc, recursive = TRUE)
  file.create(paste(meme_loc, "ame", sep = "/"))
  file.create(paste(meme_loc, "dreme", sep = "/"))
  dummy_meme <- TRUE
}


## -----------------------------------------------------------------------------
search_meme_path <- cmd_path_search(default_path = "~/meme/bin")

search_meme_path()

## ---- error = TRUE------------------------------------------------------------
search_meme_path("bad/path")

## -----------------------------------------------------------------------------
search_meme_path <- cmd_path_search(environment_var = "MEME_PATH")

## ---- error = T---------------------------------------------------------------
# Without environment variable defined
search_meme_path()

## -----------------------------------------------------------------------------
# With environment variable defined
Sys.setenv("MEME_PATH" = "~/meme/bin")
search_meme_path()

## -----------------------------------------------------------------------------
search_meme_path <- cmd_path_search(environment_var = "MEME_PATH",
                                       default_path = "~/meme/bin")

## -----------------------------------------------------------------------------
Sys.setenv("MEME_PATH" = "bad/path")
search_meme_path()

## ---- error = TRUE------------------------------------------------------------
search_meme_path(path = "bad/path")

## -----------------------------------------------------------------------------
search_meme_path <- cmd_path_search(environment_var = "MEME_PATH",
                                       default_path = "~/meme/bin",
                                       utils = c("dreme", "ame"))

## ---- error = T---------------------------------------------------------------
search_meme_path("bad/path")

## ---- error = T---------------------------------------------------------------
search_meme_path(util = "dreme")

## -----------------------------------------------------------------------------
check_meme_install <- function(path = NULL){
  cmd_install_check(search_meme_path, path = path)
}

## -----------------------------------------------------------------------------
# searches default meme search locations
check_meme_install()

## -----------------------------------------------------------------------------
# uses user override
check_meme_install('bad/path')

## -----------------------------------------------------------------------------
cmd_ui_file_exists("bad/file")
cmd_ui_file_exists("~/meme/bin")

## -----------------------------------------------------------------------------
meme_installed <- cmd_install_is_valid(search_meme_path)
meme_installed()

## -----------------------------------------------------------------------------
ame_installed <- cmd_install_is_valid(search_meme_path, util = "ame")
ame_installed()

## -----------------------------------------------------------------------------
search_meme_path <- cmd_path_search(environment_var = "MEME_PATH",
                                       default_path = "~/meme/bin",
                                       utils = c("dreme", "ame"))

runDreme <- function(..., meme_path = NULL){
  flags <- cmd_args_dots() %>% 
    cmd_list_interp() %>% 
    cmd_list_to_flags()
  
  dreme_path <- search_meme_path(path = meme_path, util = "dreme")
  
  system2(dreme_path, flags)
}

## ---- eval=FALSE--------------------------------------------------------------
#  runDreme(version = TRUE)

## ---- echo = FALSE------------------------------------------------------------
cat("5.1.1")

## -----------------------------------------------------------------------------
myFunction <- function(arg1, arg2, someText = "default"){
  flags <- cmd_args_named(keep = c("arg1", "arg2")) %>% 
    cmd_list_interp() %>% 
    cmd_list_to_flags()
  
  print(someText)
  
  return(flags)
}

myFunction(arg1 = "blah", arg2 = "blah")

## -----------------------------------------------------------------------------
myFunction(arg1 = "blah", arg2 = "blah", someText = "hello world")

## -----------------------------------------------------------------------------
myList <- list('value1' = TRUE,
               'value2' = "Hello",
               'value2' = 1:4)

cmd_list_keep(myList, "value2")

## -----------------------------------------------------------------------------
cmd_list_keep(myList, c("value2" = "Hello"))

## -----------------------------------------------------------------------------
cmd_list_drop(myList, "value2")

## -----------------------------------------------------------------------------
myFunction <- function(arg1, arg2){
  flags <- cmd_args_named() %>% 
    cmd_list_interp() %>% 
    # if arg2 == "baz", don't include it 
    cmd_list_drop(c("arg2" = "baz")) %>% 
    cmd_list_to_flags()
  
  return(flags)
}

myFunction(arg1 = "foo", arg2 = "bar")
myFunction(arg1 = "foo", arg2 = "baz")

## -----------------------------------------------------------------------------
cmd_file_combn(ext = c("txt", "xml"), prefix = "outFile")

## -----------------------------------------------------------------------------
cmd_file_combn(ext = "txt", prefix = c("outFile", "outFile2", "outFile3"))

## ---- error=T-----------------------------------------------------------------
user_input_flags <- c("delte")

system2("tar", "--help", stdout = TRUE) %>% 
  cmd_help_parse_flags() %>% 
  # Compares User-input flags to parsed commandline flags
  # returns flags that match based on edit distance
  cmd_help_flags_similar(user_input_flags) %>% 
  # Prints error message suggesting the most similar flag name
  cmd_help_flags_suggest()

## -----------------------------------------------------------------------------
shellCut_unsafe <- function(text, ...){

  flags <- cmd_args_dots() %>%
    cmd_list_interp() %>% 
    cmd_list_to_flags()

	system2("echo", c(text , "|", "cut", flags), stdout = TRUE)

}

shellCut_unsafe("hello_world", f = 2, d = "_ && echo unsafe operation!")

## ---- eval=F------------------------------------------------------------------
#  shellCut("hello_world", f = 2, d = "_ && rm ~/deleteme.txt")

## ----teardown, echo = F-------------------------------------------------------
if (dummy_meme) {
  unlink(meme_loc, recursive = TRUE)
}

