OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

all: transmission memory latency

pre: preprocess.py
	python preprocess.py

transmission: transmission.R pre
	Rscript transmission.R
	$(VIEWER) transmission*.png &

memory: memory.R pre
	Rscript memory.R
	$(VIEWER) memory*.png &

latency: latency.R pre
	Rscript latency.R
	$(VIEWER) latency*.png &

clean:
	rm *.png
