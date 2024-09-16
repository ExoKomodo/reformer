UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ENTRYPOINT := src/init.scm

CWD := $(shell pwd)
LIB_DIR := $(CWD)/lib
SOURCE_DIR := $(CWD)/src
GUILE_ARGS := -L $(LIB_DIR) -L $(SOURCE_DIR)

INIT_SOURCES := $(wildcard src/*.scm)

SOURCES := $(wildcard src/reformer/*.scm)

SQLITE_LIBRARY_PATH ?= libsqlite3

.ONESHELL:

##@ Project
.PHONY: run
run: $(INIT_SOURCES) $(SOURCES) ## Run the project. Assumes setup is complete.
	export SQLITE_LIBRARY_PATH=$(SQLITE_LIBRARY_PATH); \
	guile \
		$(GUILE_ARGS) \
		-s \
			$(ENTRYPOINT)

.PHONY: run-on-server
run-on-server: setup-systemd lb ## Run the project as a systemd service

.PHONY: run-with-repl
run-with-repl: REPL_PORT ?= 1689
run-with-repl: $(INIT_SOURCES) $(SOURCES) ## Run the project with a REPL server exposed
	export SQLITE_LIBRARY_PATH=$(SQLITE_LIBRARY_PATH); \
	guile \
		$(GUILE_ARGS) \
		--listen=$(REPL_PORT) \
		-s \
			$(ENTRYPOINT)

.PHONY: setup-lb
setup-lb: ## Sets up the LB and syncs static content. Needs root access.
	NGINX_CONF=$(shell nginx -V 2>&1 | grep -o '\-\-conf-path=\(.*conf\)' | cut -d '=' -f2); \
	ln -s -f $(CWD)/src/etc/nginx/nginx.conf $${NGINX_CONF}; \
	mkdir -p /var/www; \
	ln -s -f -n $(CWD)/src/www/reformer.fyi/ /var/www/reformer.fyi; \
	nginx -t

.PHONY: setup-systemd
setup-systemd: ## Sets up the systemd service for auto-restart
	SERVICE=/etc/systemd/system/reformer.service; \
	ln -s -f $(CWD)/src$${SERVICE} $${SERVICE}; \
	systemctl daemon-reload; \
	systemctl enable reformer.service; \
	systemctl start reformer.service

.PHONY: lb
lb: ## Runs the load balancer, enabling static content serving
	nginx

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
setup-arch: setup-pacman setup-lb ## Sets up an Arch machine
.PHONY: setup-debian
setup-debian: setup-apt setup-lb ## Sets up a Debian-based machine
.PHONY: setup-fedora
setup-fedora: setup-dnf setup-lb ## Sets up a Fedora machine
.PHONY: setup-osx
setup-osx: setup-brew setup-lb ## Sets up a Mac machine

##@ Package manager installs

.PHONY: setup-apt
setup-apt: ## Install packages via APT
	apt-get update -y; \
	if ! apt-get install -y guile-3.0; then \
		apt-get install -y guile; \
	fi; \
	apt-get install -y \
		nginx \
		libsqlite3-dev \
		sqlite3
.PHONY: setup-brew
setup-brew: ## Install packages via Homebrew (https://brew.sh)
	brew install \
		guile \
		nginx \
		sqlite; \
	brew link \
		--force \
		sqlite
.PHONY: setup-dnf
setup-dnf: ## Install packages via DNF
	dnf install -y \
		guile \
		nginx \
		sqlite \
		sqlite-devel
.PHONY: setup-pacman
setup-pacman: ## Install packages via Pacman
	pacman -S \
		guile \
	  	nginx \
		sqlite \
		sqlite-devel

##@ REPL

.PHONY: repl
repl: repl-with-port ## Runs a REPL that can load the project

.PHONY: repl-with-port
repl-with-port: ## Runs a REPL that can load the project, with an open REPL port
	guile \
		-L $(SOURCE_DIR) \
		--listen=$(REPL_PORT)

.PHONY: repl-no-port
repl-no-port: ## Runs a REPL that can load the project, without an open REPL port
	guile \
		-L $(SOURCE_DIR)

##@ Container

CONTAINER_NAME ?= exokomodo/reformer
CONTAINER_TAG ?= dev
ADDITIONAL_CONTAINER_BUILD_ARGS ?= 
ADDITIONAL_CONTAINER_PUSH_ARGS ?= 

.PHONY: container-build
container-build:
container-build: ## Builds the container
ifeq ($(UNAME_M), arm64)
	docker buildx build --platform linux/amd64 . \
	--tag $(CONTAINER_NAME):$(CONTAINER_TAG) \
	--load \
	$(ADDITIONAL_CONTAINER_BUILD_ARGS)
else
	docker build . \
	--tag $(CONTAINER_NAME):$(CONTAINER_TAG) \
	$(ADDITIONAL_CONTAINER_BUILD_ARGS)
endif

.PHONY: container-run
container-run:
container-run: ## Runs the container
	docker run -it \
		-p 80:80 \
		-p 8080:8080 \
		-p 1689:1689 \
		--mount type=bind,source="$(CWD)/src",target=/app/src \
		$(CONTAINER_NAME):$(CONTAINER_TAG)

.PHONY: container-push
container-push: ## Pushes a specific tagged container
	docker image push \
		$(CONTAINER_NAME):$(CONTAINER_TAG) \
		$(ADDITIONAL_CONTAINER_PUSH_ARGS)

.PHONY: container-push-all
container-push-all: ## Pushes all container tags
	docker image push \
		--all-tags \
		$(CONTAINER_NAME) \
		$(ADDITIONAL_CONTAINER_PUSH_ARGS)

##@ Help

.PHONY: help
help: ## Displays help info
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
