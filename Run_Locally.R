		#  Script to initialize experiment opinion app on individual pc's
		#  Relies on Gist for the code
		#  This file and the source code for the application located at
		#  https://github.com/Huh/Risk-Assessment
		#  The Gist upon which this script relies and calls is located at
		#  https://gist.github.com/Huh/d0c4febc57fcd1756aa0
		#  Josh Nowak
		#  11/2015
################################################################################
		#  Check that all necessary packages are installed
		pkg_lst <- c("shiny", "ggplot2", "googlesheets", "DT", "dplyr")
		new_pkgs <- pkg_lst[!(pkg_lst %in% installed.packages()[,"Package"])]
		if(length(new_pkgs)){ 
			install.packages(new_pkgs)		
		}
		
################################################################################		
		#  Run app locally from Gist - this should open web browser and 
		#   application
		shiny::runGist("https://gist.github.com/Huh/d0c4febc57fcd1756aa0")
		
		#  If the above worked, stop here
		
################################################################################
		#  We can also source an app directly from GitHub
		shiny::runGitHub("Risk-Assessment", "Huh")	

################################################################################
		#  Finally, we can download a zip file from the following url and run it
		#  directly
		runUrl("https://dl.dropboxusercontent.com/u/7539703/Risk-Assessment.zip")
		
################################################################################
		#  End