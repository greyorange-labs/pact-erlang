CC := gcc
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)
ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
LIBRARY_BASE_PATH := $(BASEDIR)/priv
PACT_INCLUDE_PATH := $(CURDIR)/include

UNAME := $(shell uname -s)
ARCHITECTURE := $(shell uname -m)

PACT_FFI_VERSION  := 0.4.7
PACT_FFI_BASE_URL := https://github.com/pact-foundation/pact-reference/releases/download/libpact_ffi-v$(PACT_FFI_VERSION)
PACT_H_URL        := $(PACT_FFI_BASE_URL)/pact.h

ifeq ($(UNAME),Linux)
    CFLAGS := -O2 -Wmissing-prototypes -Wall -shared -o
    ifeq ($(ARCHITECTURE), arm64)
        PACT_FILE_NAME := libpact_ffi-linux-aarch64.a
    endif
	ifeq ($(ARCHITECTURE), aarch64)
        PACT_FILE_NAME := libpact_ffi-linux-aarch64.a
    endif
    ifeq ($(ARCHITECTURE), x86_64)
        PACT_FILE_NAME := libpact_ffi-linux-x86_64.a
    endif
endif
ifeq ($(UNAME),Darwin)
    CFLAGS := -undefined dynamic_lookup -shared -o
    ifeq ($(ARCHITECTURE), arm64)
        PACT_FILE_NAME := libpact_ffi-osx-aarch64-apple-darwin.a
    endif
    ifeq ($(ARCHITECTURE), x86_64)
        PACT_FILE_NAME := libpact_ffi-osx-x86_64.a
    endif
endif
PACT_FFI_URL := $(PACT_FFI_BASE_URL)/$(PACT_FILE_NAME).gz

SRC := pactffi_nif.c
TARGET := $(BASEDIR)/priv/libpact_ffi.so
LIBRARY_PATH := $(LIBRARY_BASE_PATH)/$(PACT_FILE_NAME)
PACT_VERSION_FILE := $(LIBRARY_BASE_PATH)/.pact-ffi-version-$(PACT_FFI_VERSION)

all: $(TARGET) compile_commands.json
	@:

# Generate compile_commands.json without using external commands
compile_commands.json: Makefile
	@echo "[{" > compile_commands.json
	@echo "  \"directory\": \"$(CURDIR)\"," >> compile_commands.json
	@echo "  \"file\": \"$(CURDIR)/$(SRC)\"," >> compile_commands.json
	@echo "  \"command\": \"$(CC) $(CFLAGS) $(TARGET) $(SRC) -I$(ERTS_INCLUDE_DIR) -I$(PACT_INCLUDE_PATH) $(LIBRARY_PATH)\"" >> compile_commands.json
	@echo "}]" >> compile_commands.json

$(TARGET): $(SRC) $(LIBRARY_PATH)
	@$(CC) $(CFLAGS) $(TARGET) $(SRC) -I$(ERTS_INCLUDE_DIR) -I$(PACT_INCLUDE_PATH) $(LIBRARY_PATH)

$(LIBRARY_PATH): $(PACT_VERSION_FILE)
	@mkdir -p $(LIBRARY_BASE_PATH)
	@rm -f $(LIBRARY_PATH)
	@rm -f $(LIBRARY_PATH).gz
	@echo "Fetching libpact_ffi..."
	@curl -sS -L $(PACT_FFI_URL) -o $(LIBRARY_PATH).gz
	@gzip -d $(LIBRARY_PATH).gz
	@cp $(BASEDIR)/src/pact_escript.escript $(LIBRARY_BASE_PATH)
	@echo "Done"

$(PACT_VERSION_FILE):
	@mkdir -p $(LIBRARY_BASE_PATH)
	@rm -f $(LIBRARY_BASE_PATH)/.pact-ffi-version-*
	@touch $(PACT_VERSION_FILE)

clean:
	@rm -f $(TARGET)
	@rm -f compile_commands.json
	@rm -f $(LIBRARY_PATH)
	@rm -f $(PACT_VERSION_FILE)

update-pact-header:
	@rm -f $(PACT_INCLUDE_PATH)/pact.h
	@echo "Fetching pact.h..."
	@curl -sS -L $(PACT_H_URL) -o $(PACT_INCLUDE_PATH)/pact.h
	@echo "Done"
	@echo ">> Please commit the include/pact.h file if there are any updates"

.PHONY: all clean update-pact-header
