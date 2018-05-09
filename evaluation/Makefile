OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: disc show

all: pre default

pre: preprocess.py
	rm -rf processed
	python preprocess.py

disc: pre
	Rscript disc18/plot0.R
	Rscript disc18/plot1.R
	Rscript disc18/plot2.R
	Rscript disc18/plot3.R
	Rscript disc18/plot4.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
