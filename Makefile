# The makefile must be run from the project directory so that Rscript can pick up
# the renv. 

# Define the pattern for the subdirectories
MODULES := $(shell find . -type d -name 'Module-*')

# Find all .Rmd files in the Module-* directories
RMD_FILES := $(shell find $(MODULES) -type f -name '*.Rmd')

# Define the target for rendering all .Rmd files
all: render


# Rule to render .Rmd files
render: $(RMD_FILES:.Rmd=.pdf)

# Rule to generate PDF from Rmd
%.pdf: %.Rmd
	  @echo "Rendering $< to $@";
	  Rscript -e "renv::run(rmarkdown::render(normalizePath('$<', winslash='/')))"

# Clean up generated files with associated .Rmd files
clean:
	@echo "Cleaning up..."
	@for file in $(RMD_FILES); do \
	  pdf_file=$$(echo $$file | sed 's/\.Rmd$$/.pdf/'); \
	  tex_file=$$(echo $$file | sed 's/\.Rmd$$/.tex/'); \
	  cache_dir=$$(echo $$file | sed 's/\.Rmd$$/_cache/'); \
	  files_dir=$$(echo $$file | sed 's/\.Rmd$$/_files/'); \
	  if [ -f $$pdf_file ]; then \
	    echo "Removing $$pdf_file and intermediate files"; \
	    rm $$pdf_file; \
	    rm $$tex_file; \
	    rm -rf $$cache_dir; \
	    rm -rf $$files_dir; \
	  fi; \
	done

.PHONY: all render clean
