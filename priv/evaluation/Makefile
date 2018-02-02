OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

all: transmission

pre: preprocess.py
	python preprocess.py

split:
	rm -rf processed_break_link
	mkdir processed_break_link
	mv processed/*True~delta* processed_break_link/

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
