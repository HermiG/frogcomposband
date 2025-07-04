# File: Makefile

ifeq ($(OS),Windows_NT)
	include Makefile.default
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		include Makefile.osx
	else
		include Makefile.default
	endif
endif
