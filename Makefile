UNAME_S := $(shell uname -s)

ENTRYPOINT := src/init.scm

SOURCE_DIR := $(shell pwd)/src

INIT_SOURCES := $(wildcard src/*.scm)

SOURCES := $(wildcard src/reformer/*.scm)

##@ Project
.PHONY: run
run: $(INIT_SOURCES) $(SOURCES) ## Run the project. Assumes setup is complete.
	guile \
	-L $(SOURCE_DIR) \
	-s \
		$(ENTRYPOINT)

##@ Setup
.PHONY: setup
setup: ## Detects the OS and runs the appropriate setup
ifeq ($(UNAME_S), Linux)
    ifeq ($(shell cat /etc/os-release | grep '^ID=' | cut -d'=' -f2), arch)
	$(MAKE) setup-arch
    else ifeq ($(shell cat /etc/os-release | grep '^ID=' | cut -d'=' -f2), fedora)
	$(MAKE) setup-fedora
    else ifeq ($(shell cat /etc/os-release | grep '^ID=' | cut -d'=' -f2), ubuntu)
	$(MAKE) setup-debian
    else ifeq ($(shell cat /etc/os-release | grep '^ID=' | cut -d'=' -f2), debian)
	$(MAKE) setup-debian
    else
	@echo "Unsupported Linux distribution"
    endif
else ifeq ($(UNAME_S), Darwin)
	$(MAKE) setup-osx
else
	@echo "Unsupported OS"
endif

.PHONY: setup-arch
setup-arch: guile-pacman ## Sets up an Arch machine
.PHONY: setup-debian
setup-debian: guile-apt ## Sets up a Debian-based machine
.PHONY: setup-fedora
setup-fedora: guile-dnf ## Sets up a Fedora machine
.PHONY: setup-osx
setup-osx: guile-brew ## Sets up a Mac machine

.PHONY: guile-apt
guile-apt: ## Install guile via APT
	sudo apt-get install -y guile
.PHONY: guile-brew
guile-brew: ## Install guile via Homebrew (https://brew.sh)
	brew install guile
.PHONY: guile-dnf
guile-dnf: ## Install guile via DNF
	sudo dnf install -y guile
.PHONY: guile-pacman
guile-pacman: ## Install guile via Pacman
	sudo pacman -S guile

##@ Utility

.PHONY: help
help: ## Displays help info
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
