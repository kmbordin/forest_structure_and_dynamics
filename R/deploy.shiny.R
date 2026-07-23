# deploying shiny app

# install.packages('rsconnect')
# No console R:
renv::deactivate()
# Confirme que foi desativado
renv::status()
# Remova tudo relacionado ao renv
unlink("renv", recursive = TRUE)
unlink("renv.lock")
unlink(".Rprofile")

# Verifique se foi removido
list.files(all.files = TRUE, pattern = "renv")
# Deve retornar character(0) ou vazio

# Defina a opção ANTES do deploy
options(rsconnect.renv.enabled = FALSE)

# Verifique se a opção foi configurada
getOption("rsconnect.renv.enabled")
# Deve retornar FALSE

# Instale o BiocManager se não tiver
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")

# Configure os repositórios
options(repos = BiocManager::repositories())

# Verifique
getOption("repos")

options(
  repos = c(CRAN = "https://cloud.r-project.org/"),
  install.packages.compile.from.source = "never",
  rsconnect.renv.enabled = FALSE
)

if (!require("terra", quietly = TRUE)) {
  install.packages("terra", type = "binary")
}
library(tidyverse)
library(BIOMASS)
library(vegan)
library(FD)
library(hillR)


"renv" %in% installed.packages()[, "Package"] #deve ser true
renv::status() #nao deve estar ativado
#BiocManager::install("BiocVersion", update = TRUE,force = TRUE)


# Faça o deploy
rsconnect::deployApp(
  appName = "forest_structure_and_dynamics",
  forceUpdate = TRUE)



rsconnect::setAccountInfo(name='kmbordin', token='B0FAACE585D7DF8F8517782D573C0F0C', secret='lkjoUUb9L4LKOZvOLfqTBdWIKk57nrZRS+VZM1LT')

rsconnect::deployApp(appDir = "forest_structure_and_dynamics")
rsconnect::showLogs() 

