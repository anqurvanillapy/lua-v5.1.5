# makefile for building Lua
# see ../INSTALL for installation instructions
# see ../Makefile and luaconf.h for further customization

# == CHANGE THE SETTINGS BELOW TO SUIT YOUR ENVIRONMENT =======================

# FIXME(anqur): I have to use this to suppress IDE warnings, forgive me.
CC= /opt/homebrew/opt/llvm/bin/clang
CFLAGS= -std=c23 \
	-g -O2 \
	-fsanitize=address \
	-Werror -Wall -Wextra -Wpedantic \
	-DLUA_USE_LINUX -DLUA_USER_H='"ltests.h"'
AR= ar rcu
RANLIB= ranlib
RM= rm -f
LIBS= -fsanitize=address \
	-lm -lreadline

# == END OF USER SETTINGS. NO NEED TO CHANGE ANYTHING BELOW THIS LINE =========

LUA_A=	liblua.a
CORE_O=	api.o lcode.o ldebug.o ldo.o ldump.o lfunc.o lgc.o llex.o lmem.o \
	object.o lopcodes.o parser.o lstate.o intern.o ltable.o ltm.o  \
	lundump.o lvm.o lzio.o ltests.o
LIB_O=	lauxlib.o lbaselib.o ldblib.o liolib.o lmathlib.o loslib.o ltablib.o \
	lstrlib.o loadlib.o linit.o

LUA_T=	lua
LUA_O=	lua.o

LUAC_T=	luac
LUAC_O=	luac.o print.o

ALL_O= $(CORE_O) $(LIB_O) $(LUA_O) $(LUAC_O)
ALL_T= $(LUA_A) $(LUA_T) $(LUAC_T)
ALL_A= $(LUA_A)

all:	$(ALL_T)
.PHONY: all

o:	$(ALL_O)
.PHONY: o

a:	$(ALL_A)
.PHONY: a

$(LUA_A): $(CORE_O) $(LIB_O)
	$(AR) $@ $(CORE_O) $(LIB_O)	# DLL needs all object files
	$(RANLIB) $@

$(LUA_T): $(LUA_O) $(LUA_A)
	$(CC) -o $@ $(MYLDFLAGS) $(LUA_O) $(LUA_A) $(LIBS)

$(LUAC_T): $(LUAC_O) $(LUA_A)
	$(CC) -o $@ $(MYLDFLAGS) $(LUAC_O) $(LUA_A) $(LIBS)

clean:
	$(RM) $(ALL_T) $(ALL_O)
.PHONY: clean
