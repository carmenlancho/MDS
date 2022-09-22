# prueba2d9ddddddddddddddd
install.packages('credentials')
install.packages('gitcreds')
library(credentials)
library(gitcreds)

credentials::set_github_pat()
gitcreds::gitcreds_set()

