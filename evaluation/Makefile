OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: plots show

all: pre plots

pre: preprocess.py
	rm -rf processed
	python preprocess.py

plots:
	-Rscript ipdps19/first.R
	-Rscript ipdps19/second.R
	-Rscript ipdps19/plot0.R
	-Rscript ipdps19/plot2.R
	-Rscript ipdps19/plot3.R
	-Rscript ipdps19/plot6.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
