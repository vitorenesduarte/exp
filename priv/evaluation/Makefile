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
	$(VIEWER) *transmission*.pdf &

memory: memory.R pre
	Rscript memory.R
	$(VIEWER) *memory*.pdf &

latency: latency.R pre
	Rscript latency.R
	$(VIEWER) *latency*.pdf &

clean:
	rm *.pdf
