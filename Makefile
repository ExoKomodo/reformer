.DEFAULT_GOAL := help

##@ Setup
setup-osx: sbcl-brew quicklisp slime ## Sets up a Mac

sbcl-brew: ## Install SBCL via Homebrew (https://brew.sh)
	brew install sbcl

quicklisp: ## Install Quicklisp
	if [[ -d ~/.quicklisp ]]; then \
		exit 0; \
	fi; \
	curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp; \
	sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
		--eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
		--eval '(ql:add-to-init-file)' \
		--quit

slime: ## Install SLIME
	sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit

deps: quicklisp
	sbcl \
		--eval '(ql:quickload :hunchentoot)' \
		--eval '(ql:quickload :cl-who)'

##@ Utility
help: ## Displays help info
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)
