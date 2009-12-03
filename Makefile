MAKE = make --no-print-directory 

export ERL_TOP = $(shell pwd)
export EBIN = $(ERL_TOP)/ebin
export PRIV_DIR = $(ERL_TOP)/priv
#export DEBUG = 1

OUT_DIR = $(ERL_TOP)/out

.PHONY: dialyzer clean release

all: libs

libs:
	@cd lib && $(MAKE) opt

clean:
	@cd lib && $(MAKE) clean

dialyzer:
	dialyzer $(DIALYZERFLAGS) 

$(OUT_DIR):
	-@mkdir -p $(OUT_DIR)
