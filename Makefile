all: output/figure2.pdf

output/figure2.pdf: analysis.R
	Rscript -e "source('$<')"

clean:
	rm -r output

.PHONY: all clean
