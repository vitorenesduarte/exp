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

plots: micro retwis

micro:
	Rscript icde19/first.R
	Rscript icde19/second.R
	Rscript icde19/gset_gcounter.R
	Rscript icde19/gmap.R
	Rscript icde19/memory.R
	Rscript icde19/metadata.R

retwis:
	Rscript icde19/retwis.R
	Rscript icde19/retwis_processing.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
