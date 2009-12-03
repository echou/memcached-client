#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# Make include file for otp
#
# Copyright (C) 1996, Ericsson Telecommunications
# Author: Lars Thorsen
# ----------------------------------------------------
.SUFFIXES: .erl .beam .yrl .xrl .bin .mib .hrl .sgml .xml .html .ps \
	.3 .1 .fig .dvi .tex .class .java .pdf .psframe .pscrop .el .elc

# ----------------------------------------------------
#	Common macros
# ----------------------------------------------------
DEFAULT_TARGETS =  opt debug release release_docs clean docs

# ----------------------------------------------------
#	Command macros
# ----------------------------------------------------
INSTALL         = /usr/bin/install -c
INSTALL_DIR     = /usr/bin/install -c -d
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT  = ${INSTALL}
INSTALL_DATA    = ${INSTALL} -m 644

CC = gcc
HCC = $(CC)
CC32 = gcc
CFLAGS32 = -g -O2 -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64  -D_GNU_SOURCE
BASIC_CFLAGS = -g -O2 -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64  -D_GNU_SOURCE
DEBUG_FLAGS =  -g
LD = $(CC)
RANLIB = ranlib
AR = ar
PERL = /usr/bin/perl

BITS64 = 

OTP_RELEASE = 

# ----------------------------------------------------
#	Erlang language section
# ----------------------------------------------------
EMULATOR = beam
ERL_COMPILE_FLAGS += +debug_info -I $(ERL_TOP)/lib/misc/include
ifdef DEBUG
	ERL_COMPILE_FLAGS += -DDEBUG_ON
endif

ifdef NATIVE
	ERL_COMPILE_FLAGS += +native +"{hipe,[o2]}"
endif

ifdef INLINE
	ERL_COMPILE_FLAGS += +inline
endif


ERLC_WFLAGS = -W
ERLC = erlc $(ERLC_WFLAGS) $(ERLC_FLAGS)
ERL = erl -boot start_clean

ifndef EBIN
EBIN = ../ebin
endif

ifndef PRIV_DIR
PRIV_DIR = ../priv
endif

# Generated (non ebin) files...
ifndef EGIN
EGEN = .
endif

ifndef ESRC
ESRC = .
endif

$(EBIN)/%.beam: $(EGEN)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -pz $(EBIN) -o$(EBIN) $<

$(EBIN)/%.beam: $(ESRC)/%.erl Makefile
	$(ERLC) $(ERL_COMPILE_FLAGS) -pz $(EBIN) -o$(EBIN) $<

.erl.beam:
	$(ERLC) $(ERL_COMPILE_FLAGS) -pz $(dir $@) -o $(dir $@) $<

#
# When .erl files are automatically created GNU make removes them if
# they were the result of a chain of implicit rules. To prevent this
# we say that all .erl files are "precious".
#
.PRECIOUS: %.erl

## Uncomment these lines and add .idl to suffixes above to have erlc 
## eat IDL files
##$(EGEN)/%.erl: $(ESRC)/%.idl
##	$(ERLC) $(IDL_FLAGS) $<

$(EGEN)/%.erl: $(ESRC)/%.yrl
	$(ERLC) $(YRL_FLAGS) -o$(EGEN) $<

$(EGEN)/%.erl: $(ESRC)/%.xrl
	$(ERLC) $(XRL_FLAGS) -o$(EGEN) $<

