.PHONY: buildsite check clean coverage docs getwd initialize install load render setwd start test usegit
.DEFAULT_GOAL := help

define BROWSER_PYSCRIPT
import os, webbrowser, sys
from urllib.request import pathname2url

# The input is expected to be the full HTML filename
filename = sys.argv[1]
filepath = os.path.abspath(os.path.join("./vignettes/", filename))
webbrowser.open("file://" + pathname2url(filepath))
endef
export BROWSER_PYSCRIPT

define PRINT_HELP_PYSCRIPT
import re, sys

for line in sys.stdin:
	match = re.match(r'^([a-zA-Z_-]+):.*?## (.*)$$', line)
	if match:
		target, help = match.groups()
		print("%-20s %s" % (target, help))
endef
export PRINT_HELP_PYSCRIPT

BROWSER := python3 -c "$$BROWSER_PYSCRIPT"

buildsite: setwd ## create a website for the package
	Rscript -e "pkgdown::build_site('.')"

check: clean setwd ## check package
	Rscript -e "try(devtools::check('.'), silent=FALSE)"

clean: ## remove all build, and artifacts
	rm -f .Rhistory
	rm -f *.RData
	rm -f *.Rproj
	rm -rf .Rproj.user

coverage: ## get test coverage
	Rscript -e "devtools::test_coverage('.')"

create: setwd ## create a new package in current directory
	Rscript -e "usethis::create_package(path = getwd(), rstudio = FALSE)"
	rm -f .here

docs: clean setwd ## generate docs		
	Rscript -e "devtools::document('.')"

getwd: ## get current directory
	Rscript -e "getwd()"

install: clean setwd ## install package
	Rscript -e "try(devtools::install('.'), silent = FALSE)"	

initialize: setwd ## initialize: install packages devtools, usethis, pkgdown and rmarkdown
	Rscript -e "utils::install.packages(c('devtools', 'usethis', 'pkgdown', 'rmarkdown'), repos='https://cloud.r-project.org')"

help: ## print menu with all options
	@python3 -c "$$PRINT_HELP_PYSCRIPT" < $(MAKEFILE_LIST)

load: clean setwd ## load all (when developing the package)
	Rscript -e "devtools::load_all('.')"

render: ## run R markdown file in /vignettes, open rendered HTML file in the browser
	@read -p "Enter the name of the Rmd file (without extension): " filename; \
	Rscript -e "rmarkdown::render(paste0('./vignettes/', '$$filename', '.Rmd'))"; \
	python3 -c "$$BROWSER_PYSCRIPT" "$$filename.html"

setwd: ## set working directory to current directory
	Rscript -e "setwd('.')"

start: ## start or restart R session
	Rscript -e "system('R')"

test: ## runs the the package tests
	Rscript -e "devtools::test('.')"	

usegit: ## initialize Git repo and initial commit
	@read -p "Enter the first commit message: " message; \
	Rscript -e "usethis::use_git('$$message')"