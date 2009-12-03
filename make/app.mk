ifndef ERL_FILES
ERL_FILES = $(wildcard *.erl)
endif

MODULES = $(ERL_FILES:%.erl=%)
ALL_MODULES = $(shell echo $(MODULES) | tr " " ,)

TARGET_FILES = $(MODULES:%=$(EBIN)/%.beam)

include $(ERL_TOP)/make/otp.mk

ifdef APP_NAME
	APP_TARGET = $(EBIN)/$(APP_NAME).app
	APP_SRC = $(APP_NAME).app.src
else
	APP_TARGET =
	APP_SRC =
endif

debug opt: $(EBIN) $(TARGET_FILES) $(APP_TARGET)
	
clean:
	-@rm -rf $(EBIN)/*

$(EBIN):
	-@mkdir -p $(EBIN)

ifdef APP_TARGET

include ../vsn.mk

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' -e 's;%MODULES%;$(ALL_MODULES);' $< > $@
endif
