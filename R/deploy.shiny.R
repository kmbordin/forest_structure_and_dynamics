# deploying shiny app

# install.packages('rsconnect')

rsconnect::setAccountInfo(name='kmbordin', token='B0FAACE585D7DF8F8517782D573C0F0C', secret='lkjoUUb9L4LKOZvOLfqTBdWIKk57nrZRS+VZM1LT')

rsconnect::deployApp(appDir = "forest_structure_and_dynamics")
rsconnect::showLogs() 
