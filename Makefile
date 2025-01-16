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
CORE_O=	lapi.o lcode.o ldebug.o ldo.o ldump.o lfunc.o lgc.o llex.o lmem.o \
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

depend:
	@$(CC) $(CFLAGS) -MM l*.c print.c
.PHONY: depend

echo:
	@echo "CC = $(CC)"
	@echo "CFLAGS = $(CFLAGS)"
	@echo "AR = $(AR)"
	@echo "RANLIB = $(RANLIB)"
	@echo "RM = $(RM)"
.PHONY: echo

# Use `make depend` to generate the following targets.

lapi.o: lapi.c lua.h luaconf.h ltests.h intern.h lgc.h object.h limits.h \
  lstate.h ltm.h lzio.h lmem.h lapi.h ldebug.h ldo.h lfunc.h ltable.h \
  lundump.h lvm.h
lauxlib.o: lauxlib.c lua.h luaconf.h ltests.h lauxlib.h
lbaselib.o: lbaselib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
lcode.o: lcode.c lua.h luaconf.h ltests.h lcode.h llex.h lzio.h lmem.h \
  limits.h object.h lopcodes.h parser.h ldebug.h lstate.h ltm.h lgc.h \
  ltable.h
ldblib.o: ldblib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
ldebug.o: ldebug.c lua.h luaconf.h ltests.h lapi.h object.h limits.h \
  ldebug.h lstate.h ltm.h lzio.h lmem.h ldo.h lfunc.h lgc.h lopcodes.h \
  ltable.h lvm.h
ldo.o: ldo.c lua.h luaconf.h ltests.h intern.h lgc.h object.h limits.h \
  lstate.h ltm.h lzio.h lmem.h ldebug.h ldo.h lfunc.h lopcodes.h \
  ltable.h lundump.h lvm.h parser.h
ldump.o: ldump.c lua.h luaconf.h ltests.h lstate.h ltm.h object.h \
  limits.h lzio.h lmem.h lundump.h
lfunc.o: lfunc.c lua.h luaconf.h ltests.h lfunc.h object.h limits.h lgc.h \
  lmem.h lstate.h ltm.h lzio.h
lgc.o: lgc.c lua.h luaconf.h ltests.h intern.h lgc.h object.h limits.h \
  lstate.h ltm.h lzio.h lmem.h ldo.h lfunc.h ltable.h
linit.o: linit.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
liolib.o: liolib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
llex.o: llex.c lua.h luaconf.h ltests.h intern.h lgc.h object.h limits.h \
  lstate.h ltm.h lzio.h lmem.h ldo.h llex.h ltable.h parser.h
lmathlib.o: lmathlib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
lmem.o: lmem.c lua.h luaconf.h ltests.h ldebug.h lstate.h ltm.h object.h \
  limits.h lzio.h lmem.h ldo.h
loadlib.o: loadlib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
lopcodes.o: lopcodes.c lopcodes.h limits.h lua.h luaconf.h ltests.h
loslib.o: loslib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
lstate.o: lstate.c lua.h luaconf.h ltests.h intern.h lgc.h object.h \
  limits.h lstate.h ltm.h lzio.h lmem.h ldebug.h ldo.h lfunc.h llex.h \
  ltable.h
lstrlib.o: lstrlib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
ltable.o: ltable.c lua.h luaconf.h ltests.h ldebug.h lstate.h ltm.h \
  object.h limits.h lzio.h lmem.h lgc.h ltable.h
ltablib.o: ltablib.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
ltests.o: ltests.c lua.h luaconf.h ltests.h intern.h lgc.h object.h \
  limits.h lstate.h ltm.h lzio.h lmem.h lapi.h lauxlib.h ldebug.h ldo.h \
  lfunc.h lopcodes.h ltable.h lualib.h
ltm.o: ltm.c lua.h luaconf.h ltests.h intern.h lgc.h object.h limits.h \
  lstate.h ltm.h lzio.h lmem.h ltable.h
lua.o: lua.c lua.h luaconf.h ltests.h lauxlib.h lualib.h
luac.o: luac.c lauxlib.h lua.h luaconf.h ltests.h intern.h lgc.h object.h \
  limits.h lstate.h ltm.h lzio.h lmem.h ldo.h lfunc.h lopcodes.h \
  lundump.h
lundump.o: lundump.c lua.h luaconf.h ltests.h intern.h lgc.h object.h \
  limits.h lstate.h ltm.h lzio.h lmem.h ldebug.h ldo.h lfunc.h lundump.h
lvm.o: lvm.c lua.h luaconf.h ltests.h intern.h lgc.h object.h limits.h \
  lstate.h ltm.h lzio.h lmem.h ldebug.h ldo.h lfunc.h lopcodes.h \
  ltable.h lvm.h
lzio.o: lzio.c lua.h luaconf.h ltests.h limits.h lmem.h lstate.h ltm.h \
  object.h lzio.h
print.o: print.c ldebug.h lstate.h lua.h luaconf.h ltests.h ltm.h \
  object.h limits.h lzio.h lmem.h lopcodes.h lundump.h
