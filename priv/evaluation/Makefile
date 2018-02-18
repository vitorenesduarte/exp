OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: europar show

all: pre default

pre: preprocess.py
	rm -rf processed
	python preprocess.py

europar:
	Rscript europar18/plot0.R
	Rscript europar18/plot1.R
	Rscript europar18/plot2.R
	Rscript europar18/plot3.R
	Rscript europar18/plot4.R
	Rscript europar18/plot5.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
