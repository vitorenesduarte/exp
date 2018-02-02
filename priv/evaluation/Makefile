OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

all: pre split transmission show

pre: preprocess.py
	python preprocess.py

split:
	rm -rf processed_break_link
	mkdir processed_break_link
	mv processed/*True~delta* processed_break_link/

partition: partition.R
	Rscript partition.R

transmission: transmission.R
	Rscript transmission.R

memory: memory.R
	Rscript memory.R

latency: latency.R
	Rscript latency.R

show:
	$(VIEWER) *.png &

clean:
	rm *.png
