.DEFAULT_GOAL := help

UNAME_S := $(shell uname -s)

##@ Project
.PHONY: run
run: ## Run the project. Assumes setup is complete.
	sbcl \
		--script ./init.lisp

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
setup-arch: sbcl-pacman quicklisp slime ## Sets up an Arch machine
.PHONY: setup-debian
setup-debian: sbcl-apt quicklisp slime ## Sets up a Debian-based machine
.PHONY: setup-fedora
setup-fedora: sbcl-dnf quicklisp slime ## Sets up a Fedora machine
.PHONY: setup-osx
setup-osx: sbcl-brew quicklisp slime ## Sets up a Mac machine

.PHONY: sbcl-apt
sbcl-apt: ## Install SBCL via APT
	sudo apt-get install -y sbcl
.PHONY: sbcl-brew
sbcl-brew: ## Install SBCL via Homebrew (https://brew.sh)
	brew install sbcl
.PHONY: sbcl-dnf
sbcl-dnf: ## Install SBCL via DNF
	sudo dnf install -y sbcl
.PHONY: sbcl-pacman
sbcl-pacman: ## Install SBCL via Pacman
	sudo pacman -S sbcl

.PHONY: quicklisp
quicklisp: ## Install Quicklisp
	if [ -d ~/.quicklisp ]; then \
		exit 0; \
	fi; \
	curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp; \
	sbcl \
		--no-sysinit --no-userinit \
		--load /tmp/ql.lisp \
		--eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
		--eval '(ql:add-to-init-file)' \
		--quit

.PHONY: slime
slime: ## Install SLIME
	sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

##@ Utility
.PHONY: asdf-version
asdf-version: ## Print ASDF version
	sbcl \
		--noinform \
		--disable-debugger \
		--eval '(require :asdf)' \
		--eval '(format t "ASDF: ~a~%" (asdf:asdf-version))' \
		--quit

.PHONY: help
help: ## Displays help info
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
