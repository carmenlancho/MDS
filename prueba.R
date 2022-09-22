# prueba2d9dddddddddddddddff
install.packages('credentials')
install.packages('gitcreds')
library(credentials)
library(gitcreds)

credentials::set_github_pat()
gitcreds::gitcreds_set()

