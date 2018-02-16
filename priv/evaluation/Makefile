OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

default: transmission

all: pre default

pre: preprocess.py
	rm -rf processed processed_break_link
	mkdir processed 
	python preprocess.py
	mv processed processed_break_link
	mkdir processed 
	-mv processed/*none~delta* processed

partition: partition.R
	Rscript partition.R
	make show

transmission: transmission.R
	Rscript transmission.R
	make show

memory: memory.R
	Rscript memory.R
	make show

latency: latency.R
	Rscript latency.R
	make show

show:
	$(VIEWER) *.png &

clean:
	rm -f *.png
