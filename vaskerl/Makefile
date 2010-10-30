TARGET = Vaskerl

SOURCES = node.c Erlang.hs Vaskerl.hs
#OBJECTS = v_port.o Vaskerl.o

ERL_INTERFACE = /opt32/lib/erlang/lib/erl_interface-3.7.1
GHC = ghc


CFLAGS = -optc -g -optc -Wall -I$(ERL_INTERFACE)/include \
	-DCALLCONV=ccall

LDFLAGS = -L$(ERL_INTERFACE)/lib -lei -lerl_interface -package network

all: $(TARGET)
	@

$(TARGET): $(SOURCES)
	$(GHC) $(CFLAGS) -main-is $(TARGET) -o $@ $(SOURCES) $(LDFLAGS)

%.c: 

%.hs: %.hsc
	hsc2hs -I$(ERL_INTERFACE)/include $<
