# There are a couple of conventions we use so make works a little better.
#
# We sometimes want to build an entire directory of files based on one file.
# We do this for dependencies. E.g.: package.json -> node_modules.
# For these cases, we track an empty `.stamp` file in the directory.
# This allows us to keep up with make's dependency model.
#
# We also want some place to store all the excess build artifacts.
# This might be test outputs, or it could be some intermediate artifacts.
# For this, we use the `$(FORMLET_HALOGEN_BUILD_DIR)` directory.
# Assuming the different tools allow us to put their artifacts in here,
# we can clean up builds really easily: delete this directory.
#
# We use some make syntax that might be unfamiliar, a quick refresher:
# make is based on a set of rules
#
# <targets>: <prerequisites> | <order-only-prerequisites>
# 	<recipe>
#
# `<targets>` are the things we want to make. This is usually a single file,
# but it can be multiple things separated by spaces.
#
# `<prerequisites>` are the things that decide when `<targets>` is out of date.
# These are also usually files. They are separated by spaces.
# If any of the `<prerequisites>` are newer than the `<targets>`,
# the recipe is run to bring the `<targets>` up to date.
#
# `<recipe>` are the commands to run to bring the `<targets>` up to date.
# These are commands like we write on a terminal.
#
# See: https://www.gnu.org/software/make/manual/make.html#Rule-Syntax
#
# `<order-only-prerequisites>` are similar to normal `<prerequisites>`
# but they don't cause a target to be rebuilt if they're out of date.
# This is mostly useful for creating directories and whatnot.
#
# See: https://www.gnu.org/software/make/manual/make.html#Prerequisite-Types
#
# And a quick refresher on some make variables:
#
# $@ - Expands to the target we're building.
# $< - Expands to the first prerequisite of the recipe.
#
# See: https://www.gnu.org/software/make/manual/make.html#Automatic-Variables
#
# `.DEFAULT_GOAL` is the goal to use if no other goals are specified.
# Normally, the first goal in the file is used if no other goals are specified.
# Setting this allows us to override that behavior.
#
# See: https://www.gnu.org/software/make/manual/make.html#index-_002eDEFAULT_005fGOAL-_0028define-default-goal_0029
#
# `.PHONY` forces a recipe to always run. This is useful for things that are
# more like commands than targets. For instance, we might want to clean up
# all artifacts. Since there's no useful target, we can mark `clean` with
# `.PHONY` and make will run the task every time we ask it to.
#
# See: https://www.gnu.org/software/make/manual/make.html#Phony-Targets
# See: https://www.gnu.org/software/make/manual/make.html#index-_002ePHONY-1

# Absolute path to either this project, or the root project if called
# from parent Makefile
ROOT_DIR ?= $(shell pwd)

# Relative path to this directory from $ROOT_DIR
# If called from a parent Makefile, will resolve to something like `lib/formlet`

FORMLET_HALOGEN_DIR ?= .

# Library-specific constants
FORMLET_HALOGEN_BUILD_DIR := $(FORMLET_HALOGEN_DIR)/.build
FORMLET_HALOGEN_FORMAT_PURS_TIDY_STAMP := $(FORMLET_HALOGEN_BUILD_DIR)/.format-formlet-halogen-purs-tidy-stamp
FORMLET_HALOGEN_PURS := $(shell find $(FORMLET_HALOGEN_DIR) -name '*.purs' -type f)
FORMLET_HALOGEN_SPAGO_CONFIG := $(FORMLET_HALOGEN_DIR)/spago.dhall
FORMLET_HALOGEN_SRCS := $(shell find $(FORMLET_HALOGEN_DIR) $(FIND_SRC_FILE_ARGS))
ROOT_DIR_RELATIVE := $(shell echo '$(FORMLET_HALOGEN_DIR)' | sed 's \([^./]\+\) .. g')

# Variables we want to inherit from a parent Makefile if it exists
OUTPUT_DIR ?= $(ROOT_DIR)/output
SPAGO_DIR ?= $(ROOT_DIR)/.spago
SPAGO_PACKAGES_CONFIG ?= $(FORMLET_HALOGEN_DIR)/packages.dhall
SPAGO_STAMP ?= $(SPAGO_DIR)/.stamp

# Commands and variables
PSA ?= psa
PSA_ARGS ?= --censor-lib --stash=$(FORMLET_HALOGEN_BUILD_DIR)/.psa_stash --strict --is-lib=$(SPAGO_DIR) --censor-codes=HiddenConstructors
PURS_TIDY ?= purs-tidy
PURS_TIDY_CMD ?= check
RTS_ARGS ?= +RTS -N2 -A800m -RTS
SPAGO ?= spago
SPAGO_BUILD_DEPENDENCIES ?= $(SPAGO_STAMP)

# Colors for printing
CYAN ?= \033[0;36m
RESET ?= \033[0;0m

# Variables we add to
ALL_SRCS += $(FORMLET_HALOGEN_SRCS)
CLEAN_DEPENDENCIES += clean-formlet-halogen
FORMAT_DEPENDENCIES += $(FORMLET_HALOGEN_FORMAT_PURS_TIDY_STAMP)
SPAGO_CONFIGS += $(FORMLET_HALOGEN_SPAGO_CONFIG)
TEST_DEPENDENCIES += test-formlet-halogen

.DEFAULT_GOAL := build-formlet-halogen

$(FORMLET_HALOGEN_BUILD_DIR) $(OUTPUT_DIR):
	mkdir -p $@

$(FORMLET_HALOGEN_BUILD_DIR)/help-unsorted: $(MAKEFILE_LIST) | $(FORMLET_HALOGEN_BUILD_DIR)
	@grep \
		--extended-regexp '^[A-Za-z_-]+:.*?## .*$$' \
		--no-filename \
		$(MAKEFILE_LIST) \
		> $@

$(FORMLET_HALOGEN_BUILD_DIR)/help: $(FORMLET_HALOGEN_BUILD_DIR)/help-unsorted | $(FORMLET_HALOGEN_BUILD_DIR)
	@sort $< > $@

$(FORMLET_HALOGEN_FORMAT_PURS_TIDY_STAMP): $(FORMLET_HALOGEN_PURS) | $(FORMLET_HALOGEN_BUILD_DIR)
	$(PURS_TIDY) $(PURS_TIDY_CMD) $(FORMLET_HALOGEN_DIR)/src $(FORMLET_HALOGEN_DIR)/test
	@touch $@

$(FORMLET_HALOGEN_SPAGO_CONFIG): gen-spago-config-formlet-halogen $(FORMLET_HALOGEN_DIR)/spago.template.dhall

$(SPAGO_STAMP): $(SPAGO_PACKAGES_CONFIG) $(SPAGO_CONFIGS)
	# `spago` doesn't clean up after itself if different versions are installed, so we do it ourselves.
	rm -fr $(SPAGO_DIR)
	$(SPAGO) install $(RTS_ARGS)
	touch $@

.PHONY: build-formlet-halogen
build-formlet-halogen: $(FORMLET_HALOGEN_SPAGO_CONFIG) $(SPAGO_BUILD_DEPENDENCIES) | $(FORMLET_HALOGEN_BUILD_DIR) ## Build the `formlet-halogen` package
	$(SPAGO) --config $(FORMLET_HALOGEN_SPAGO_CONFIG) build --purs-args '$(PSA_ARGS) $(RTS_ARGS)'

.PHONY: check-format-formlet-halogen
check-format-formlet-halogen: PURS_TIDY_CMD=check
check-format-formlet-halogen: $(FORMLET_HALOGEN_FORMAT_PURS_TIDY_STAMP) ## Validate formatting of the `formlet-halogen` directory

# Since some of these variables are shared with the root Makefile.
# Running `clean-formlet-halogen` from root might have the unintended consequence
# of cleaning more than intended. Specifically, it will remove the
# root $OUTPUT_DIR and $SPAGO_DIR.
.PHONY: clean-formlet-halogen
clean-formlet-halogen: clean-spago-config-formlet-halogen
	rm -fr \
		$(FORMLET_HALOGEN_BUILD_DIR) \
		$(OUTPUT_DIR) \
		$(SPAGO_DIR)

.PHONY: clean-spago-config-formlet-halogen
clean-spago-config-formlet-halogen:
	rm -f $(FORMLET_HALOGEN_SPAGO_CONFIG)

.PHONY: format-formlet-halogen
format-formlet-halogen: PURS_TIDY_CMD=format-in-place
format-formlet-halogen: $(FORMLET_HALOGEN_FORMAT_PURS_TIDY_STAMP) ## Format the `formlet-halogen` directory

.PHONY: gen-spago-config-formlet-halogen
gen-spago-config-formlet-halogen: $(FORMLET_HALOGEN_DIR)/spago.template.dhall | $(FORMLET_HALOGEN_BUILD_DIR)
	@sed \
		's+{{PACKAGES_DIR}}+$(ROOT_DIR_RELATIVE)+g; s+{{SOURCES_DIR}}+$(FORMLET_HALOGEN_DIR)+g; s+{{GENERATED_DOC}}+This config is auto-generated by make.\nIf the paths are wrong, try deleting it and running make again.+g' \
		$(FORMLET_HALOGEN_DIR)/spago.template.dhall > $(FORMLET_HALOGEN_BUILD_DIR)/spago.dhall
	@if cmp -s -- '$(FORMLET_HALOGEN_BUILD_DIR)/spago.dhall' '$(FORMLET_HALOGEN_SPAGO_CONFIG)'; then \
		echo 'Nothing to do for $(FORMLET_HALOGEN_SPAGO_CONFIG)'; \
	else \
		echo 'Generating new $(FORMLET_HALOGEN_SPAGO_CONFIG)'; \
		cp $(FORMLET_HALOGEN_BUILD_DIR)/spago.dhall $(FORMLET_HALOGEN_SPAGO_CONFIG); \
	fi

.PHONY: help
help: $(FORMLET_HALOGEN_BUILD_DIR)/help ## Display this help message
	@awk 'BEGIN {FS = ":.*?## "}; {printf "$(CYAN)%-30s$(RESET) %s\n", $$1, $$2}' $<

.PHONY: test-formlet-halogen
test-formlet-halogen: $(FORMLET_HALOGEN_SPAGO_CONFIG) $(SPAGO_BUILD_DEPENDENCIES) | $(FORMLET_HALOGEN_BUILD_DIR) ## Run only formlet-halogen tests
	$(SPAGO) --config $(FORMLET_HALOGEN_SPAGO_CONFIG) test --main Test.Formlet.Halogen --purs-args '$(PSA_ARGS) $(RTS_ARGS)'

.PHONY: variable-%
variable-%: ## Display the value of a variable; e.g. `make variable-FORMLET_HALOGEN_BUILD_DIR`
	@echo '$*=$($*)'

