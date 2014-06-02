HMAKE := cabal
CWD := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

all: configure build install

configure:
	$(HMAKE) configure

build:
	$(HMAKE) build

install:
	$(HMAKE) install --prefix=$(CWD)

clean:
	$(HMAKE) clean
	rm -rfv bin share
