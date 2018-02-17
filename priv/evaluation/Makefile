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
	Rscript plot1.R

show:
	$(VIEWER) *.png

clean:
	rm -f *.png
