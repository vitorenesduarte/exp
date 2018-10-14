OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: plots show

all: pre plots show

pre: preprocess.py
	rm -rf processed
	python preprocess.py

plots:
	-Rscript icde19/first.R
	-Rscript icde19/second.R
	-Rscript icde19/gset_gcounter.R
	-Rscript icde19/gmap.R
	-Rscript icde19/memory.R
	-Rscript icde19/processing.R
	-Rscript icde19/metadata.R
	make retwis_plots

retwis_plots:
	-Rscript icde19/retwis.R
	-Rscript icde19/retwis_latency.R
	-Rscript icde19/retwis_processing.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
