OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: plots show

all: pre default

pre: preprocess.py
	rm -rf processed
	python preprocess.py

plots:
	Rscript plot0.R
	Rscript plot1.R
	Rscript plot2.R
	Rscript plot3.R
	Rscript plot4.R
	Rscript plot5.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
