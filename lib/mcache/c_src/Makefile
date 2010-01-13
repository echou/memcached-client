include $(ERL_TOP)/make/otp.mk

OBJ_DIR = ../obj
LIB_DIR = $(PRIV_DIR)/lib

ERLENV = $(shell env escript $(ERL_TOP)/make/erlenv.escript)
ERL_ROOT_DIR = $(word 1,$(ERLENV))
ERL_EI_DIR = $(word 2,$(ERLENV))

CC = g++
LD = g++
CFLAGS = -g -O2 -D_REENTRANT -DUSE_THREADS -D_GNU_SOURCE -fPIC 
LDFLAGS = -shared

ALL_CFLAGS = -I . -I $(ERL_ROOT_DIR)/usr/include -I $(ERL_EI_DIR)/include
LDLIBS = -L $(ERL_EI_DIR)/lib -L $(ERL_ROOT_DIR)/usr/lib -lei -lerl_interface 

DRIVER_OBJS = $(OBJ_DIR)/memcached_drv.o

DYN_DRIVER = $(LIB_DIR)/memcached_drv.so $(LIB_DIR)/mcache_sock.so

debug opt: $(OBJ_DIR) $(LIB_DIR) $(DYN_DRIVER) 

$(OBJ_DIR):
	-@mkdir -p $(OBJ_DIR)

$(LIB_DIR):
	-@mkdir -p $(LIB_DIR)

$(OBJ_DIR)/%.o: %.cpp termdata.hpp
	$(CC) -c -o $@ $(CFLAGS) $(ALL_CFLAGS) $<

$(LIB_DIR)/memcached_drv.so: $(DRIVER_OBJS) Makefile
	$(LD) $(LDFLAGS) -o $@ $(DRIVER_OBJS) $(LDLIBS) -lmemcached

$(LIB_DIR)/mcache_sock.so: $(OBJ_DIR)/mcache_sock.o Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_DIR)/mcache_sock.o $(LDLIBS)

clean:
	-@rm -rf $(LIB_DIR) $(OBJ_DIR)
