UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ENTRYPOINT := src/init.scm

CWD := $(shell pwd)
LIB_DIR := $(CWD)/lib/src
SOURCE_DIR := $(CWD)/src
GUILE_ARGS := -L $(LIB_DIR) -L $(SOURCE_DIR)

INIT_SOURCES := $(wildcard src/*.scm)

SOURCES := $(wildcard src/reformer/*.scm)

export DEBIAN_FRONTEND ?= noninteractive
export SQLITE_LIBRARY_PATH ?= libsqlite3
export PSQL_LIBRARY_PATH ?= libpq
export DB_IMPLEMENTATION ?= sqlite
export REFORMER_CONNECTION_STRING ?= postgresql://[user[:password]@][netloc][:port]
export REFORMER_TEST_CONNECTION_STRING ?= postgresql://[user[:password]@][netloc][:port]
export REFORMER_MAIL_PASSWORD ?= "GIVE MEMY 16DI GITS"
export GUILE_AUTO_COMPILE ?= fresh

.ONESHELL:

##@ Project
.PHONY: run
run: $(INIT_SOURCES) $(SOURCES) ## Run the project. Assumes setup is complete.
	guile \
		$(GUILE_ARGS) \
		-s \
			$(ENTRYPOINT)

.PHONY: run-on-server
run-on-server: setup-systemd lb ## Run the project as a systemd service

.PHONY: run-with-repl
run-with-repl: REPL_PORT ?= 1689
run-with-repl: $(INIT_SOURCES) $(SOURCES) ## Run the project with a REPL server exposed
	guile \
		$(GUILE_ARGS) \
		--listen=$(REPL_PORT) \
		-s \
			$(ENTRYPOINT)

.PHONY: test
test: test/auth

.PHONY: test/auth
test/auth: $(INIT_SOURCES) $(SOURCES) test/test-auth.scm
	@echo "Testing auth"; \
	guile \
		$(GUILE_ARGS) \
		-s \
			$(shell pwd)/test/test-auth.scm

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
		fetchmail \
		libpq-dev \
		libsqlite3-dev \
		libtool \
		msmtp \
		nginx \
		sqlite3;

.PHONY: setup-brew
setup-brew: ## Install packages via Homebrew (https://brew.sh)
	brew install \
		fetchmail \
		guile \
		libpq \
		msmtp \
		nginx \
		sqlite; \
	brew link \
		--force \
		--overwrite \
		libpq \
		sqlite;

.PHONY: setup-dnf
setup-dnf: ## Install packages via DNF
	dnf install -y \
		fetchmail \
		guile \
		msmtp \
		nginx \
		sqlite \
		sqlite-devel;

.PHONY: setup-pacman
setup-pacman: ## Install packages via Pacman
	pacman -S \
		fetchmail \
		guile \
		msmtp \
		nginx \
		sqlite \
		sqlite-devel;

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
container-build: container-build-$(UNAME_M) ## Builds the container (detects CPU architecture)

.PHONY: container-build-amd64
container-build-amd64: ## Builds the container (amd64)
	ADDITIONAL_CONTAINER_BUILD_ARGS="$(ADDITIONAL_CONTAINER_BUILD_ARGS) --build-arg IMAGE_NAME=debian";
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
.PHONY: container-build-x86_64
container-build-x86_64: container-build-amd64 ## Builds the container (amd64)

.PHONY: container-build-arm64
container-build-arm64: ## Builds the container (arm)
	ADDITIONAL_CONTAINER_BUILD_ARGS="$(ADDITIONAL_CONTAINER_BUILD_ARGS) --build-arg IMAGE_NAME=arm64v8/debian";
ifeq ($(UNAME_M), arm64)
	docker build . \
	--tag $(CONTAINER_NAME):$(CONTAINER_TAG) \
	$${ADDITIONAL_CONTAINER_BUILD_ARGS}
else
	docker buildx build --platform linux/arm64 . \
		--tag $(CONTAINER_NAME):$(CONTAINER_TAG) \
		--load \
		$${ADDITIONAL_CONTAINER_BUILD_ARGS}
endif

.PHONY: container-build-aarch64 ## Builds the container (arm)
container-build-aarch64: container-build-arm64
.PHONY: container-build-arm ## Builds the container (arm)
container-build-arm: container-build-arm64

.PHONY: container-run
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

##@ Helpers

.PHONY: clean
clean: ## Cleans all built files from the repo
	rm -f reformer.db; \
	rm -f test.db; \
	rm -rf ~/.cache/guile/ccache;

##@ Help

.PHONY: help
help: ## Displays help info
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
