OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

all: pre split transmission latency

pre: preprocess.py
	python preprocess.py

split:
	rm -r processed_driven
	mkdir processed_driven
	mv processed/*True~delta* processed_driven/

partition: partition.R
	Rscript partition.R
	$(VIEWER) *partition*.png &

transmission: transmission.R
	Rscript transmission.R
	$(VIEWER) *transmission*.png &

memory: memory.R
	Rscript memory.R
	$(VIEWER) *memory*.png &

latency: latency.R
	Rscript latency.R
	$(VIEWER) *latency*.png &

clean:
	rm *.png
