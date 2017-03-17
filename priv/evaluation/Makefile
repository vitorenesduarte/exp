OS := $(shell uname)
ifeq ($(OS), Linux)
	VIEWER=eog
else ifeq ($(OS), Darwin)
	VIEWER=open
endif

transmission:
	python preprocess.py
	Rscript transmission.R
	$(VIEWER) r.png &
