# FIXME(anqur): I have to use this to suppress IDE warnings, forgive me.
CC = /opt/homebrew/opt/llvm/bin/clang
CFLAGS = -std=c23 \
	-g -O2 \
	-fsanitize=address \
	-Werror -Wall -Wextra -Wpedantic \
	-DLUA_USER_H='"tests.h"'
AR = ar rcu
RANLIB = ranlib
RM = rm -f
LIBS = -fsanitize=address -lreadline

LUA_A = liblua.a
CORE_O = api.o codegen.o debug.o stack.o dump.o closure.o gc.o lexer.o \
	memory.o object.o opcodes.o parser.o state.o intern.o table.o tag.o load.o \
	vm.o buffer.o tests.o
STD_O = std.o std_builtin.o std_debug.o std_io.o std_math.o std_os.o \
	std_table.o std_string.o std_package.o

LUA_T = lua
LUA_O = lua.o

LUAC_T = luac
LUAC_O = luac.o print.o

ALL_O = $(CORE_O) $(STD_O) $(LUA_O) $(LUAC_O)
ALL_T = $(LUA_A) $(LUA_T) $(LUAC_T)
ALL_A = $(LUA_A)

all: $(ALL_T)
.PHONY: all

$(LUA_A): $(CORE_O) $(STD_O)
	$(AR) $@ $(CORE_O) $(STD_O)
	$(RANLIB) $@

$(LUA_T): $(LUA_O) $(LUA_A)
	$(CC) -o $@ $(MYLDFLAGS) $(LUA_O) $(LUA_A) $(LIBS)

$(LUAC_T): $(LUAC_O) $(LUA_A)
	$(CC) -o $@ $(MYLDFLAGS) $(LUAC_O) $(LUA_A) $(LIBS)

clean:
	$(RM) $(ALL_T) $(ALL_O)
.PHONY: clean
