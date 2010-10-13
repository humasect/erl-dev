ERL = erl
EMACS = emacs

all:
	$(ERL) -make
	make -C vaskell

docs:
	$(EMACS)
