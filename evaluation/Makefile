OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: ipdps show

all: pre default

pre: preprocess.py
	rm -rf processed
	python preprocess.py

ipdps: pre
	Rscript ipdps19/plot0.R
	Rscript ipdps19/plot1.R
	Rscript ipdps19/plot2.R
	Rscript ipdps19/plot3.R
	Rscript ipdps19/plot4.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
