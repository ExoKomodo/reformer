UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ENTRYPOINT := src/init.scm

SOURCE_DIR := $(shell pwd)/src

INIT_SOURCES := $(wildcard src/*.scm)

SOURCES := $(wildcard src/reformer/*.scm)

STATIC_DIR := www/reformer.fyi

##@ Project
.PHONY: run
run: $(INIT_SOURCES) $(SOURCES) ## Run the project. Assumes setup is complete.
	guile \
		-L $(SOURCE_DIR) \
		-s \
			$(ENTRYPOINT)

.PHONY: run-with-lb
run-with-lb: $(INIT_SOURCES) $(SOURCES) lb ## Run the project with the load balancer. Assumes setup is complete.
	guile \
		-L $(SOURCE_DIR) \
		-s \
			$(ENTRYPOINT)

.PHONY: run-with-repl-server
run-with-repl-server: REPL_PORT ?= 1689
run-with-repl-server: $(INIT_SOURCES) $(SOURCES) ## Run the project with a REPL server exposed
	guile \
		-L $(SOURCE_DIR) \
		--listen=$(REPL_PORT) \
		-s \
			$(ENTRYPOINT)

.PHONY: setup-lb
setup-lb: ## Sets up the LB and syncs static content. Needs root access.
	NGINX_CONF=$(shell nginx -V 2>&1 | grep -o '\-\-conf-path=\(.*conf\)' | cut -d '=' -f2); \
	ln -s -f $(shell pwd)/nginx/nginx.conf $${NGINX_CONF}; \
	mkdir -p /var/www/$(STATIC_DIR); \
	rsync -avh --delete $(shell pwd)/$(STATIC_DIR)/ /var/$(STATIC_DIR); \
	nginx -t

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
setup-arch: guile-pacman ## Sets up an Arch machine
.PHONY: setup-debian
setup-debian: guile-apt ## Sets up a Debian-based machine
.PHONY: setup-fedora
setup-fedora: guile-dnf ## Sets up a Fedora machine
.PHONY: setup-osx
setup-osx: guile-brew ## Sets up a Mac machine

##@ Guile install

.PHONY: guile-apt
guile-apt: ## Install guile via APT
	apt-get update -y
	if ! apt-get install -y guile-3.0; then \
		apt-get install -y guile; \
	fi
.PHONY: guile-brew
guile-brew: ## Install guile via Homebrew (https://brew.sh)
	brew install guile
.PHONY: guile-dnf
guile-dnf: ## Install guile via DNF
	dnf install -y guile
.PHONY: guile-pacman
guile-pacman: ## Install guile via Pacman
	pacman -S guile

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
		-p 88:88 \
		-p 8080:8080 \
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
