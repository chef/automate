# Global defaults
TERRAFORM_PATH = $(shell pwd)/terraform
SORT_PY = $(shell pwd)/scripts/tf_var_sort.py
AIRGAP_SH = $(shell pwd)/scripts/bundle_creation.sh
SUP_KEY_SH = $(shell pwd)/scripts/sup-keys.sh
TMPOUT = '/tmp/make.out'
REPO_ROOT = $(shell pwd)
DATESTAMP := $(shell date +"%Y%m%d%H%M%S")
BUNDLEAIB = $(TERRAFORM_PATH)/transfer_files/bundle-$(DATESTAMP).aib
BUNDLEAIBDEV = $(TERRAFORM_PATH)/transfer_files/bundle-dev-$(DATESTAMP).aib
BACKENDAIB = $(TERRAFORM_PATH)/transfer_files/backend-$(DATESTAMP).aib
BACKENDAIB_TFVARS = $(shell pwd)/terraform/a2ha_aib_be.auto.tfvars
FRONTENDAIB = $(TERRAFORM_PATH)/transfer_files/frontend-$(DATESTAMP).aib
FRONTENDAIB_TFVARS = $(shell pwd)/terraform/a2ha_aib_fe.auto.tfvars
TMPFILE := $(shell mktemp)
VARFILES := $(shell find $(TERRAFORM_PATH) -name variables.tf -o -name inputs.tf)
HABITAT_TFVARS = $(shell pwd)/terraform/a2ha_habitat.auto.tfvars

# Help menu colors
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
WHITE  := $(shell tput -Txterm setaf 7)
RESET  := $(shell tput -Txterm sgr0)

# Habitat variables - can be passed in via VARNAME=XXX make <target>
BECHANNEL?=stable
BEVERSION?=latest
FECHANNEL?=current
FEVERSION?=latest

# Targets

# https://gist.github.com/prwhite/8168133#gistcomment-2278355
# any targets that have double '#' above them will be picked up and added to the menu
## Display this help text
help:
	@echo ''
	@echo 'Usage:'
	@echo '  ${YELLOW}make${RESET} ${GREEN}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "  ${YELLOW}%-20s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
	@echo ''
	@echo 'Overrides:'
	@printf "  ${YELLOW}%-20s${RESET} ${GREEN}%s${RESET}\n" "BECHANNEL|FECHANNEL" "Bldr channel to pull from for Backend|Frontend Components. ex. BECHANNEL=acceptance make airgap"
	@printf "  ${YELLOW}%-20s${RESET} ${GREEN}%s${RESET}\n" "BEVERSION|FEVERSION" "Semver of Backend|Frontend Components to pull ex. BEVERSION=0.1.63 make airgap"
	@echo ''

$(BUNDLEAIB):
	@echo "# Updating FE and BE component package versions" && \
		$(AIRGAP_SH) -t all -o $(FRONTENDAIB) -b $(BACKENDAIB) -v $(FRONTENDAIB_TFVARS) -q $(BACKENDAIB_TFVARS)  && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(FRONTENDAIB) > $(FRONTENDAIB).md5 && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(BACKENDAIB) > $(BACKENDAIB).md5

$(BUNDLEAIBDEV):
	@echo "# Updating FE and BE component package versions from dev channel " && \
		$(AIRGAP_SH) -t all -c dev -o $(FRONTENDAIB) -b $(BACKENDAIB) -v $(FRONTENDAIB_TFVARS) -q $(BACKENDAIB_TFVARS)  && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(FRONTENDAIB) > $(FRONTENDAIB).md5 && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(BACKENDAIB) > $(BACKENDAIB).md5

$(FRONTENDAIB):
	@echo "# Updating FE component package versions: $(FRONTENDAIB_TFVARS)" && \
		$(AIRGAP_SH) -t upgradefrontends -o $(FRONTENDAIB) -v $(FRONTENDAIB_TFVARS) && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(FRONTENDAIB) > $(FRONTENDAIB).md5 

$(BACKENDAIB):
	@echo "# Updating BE component package versions: $(BACKENDAIB_TFVARS)" && \
		$(AIRGAP_SH) -t upgradebackends -b $(BACKENDAIB) -q $(BACKENDAIB_TFVARS) && \
	    $$(hab pkg path core/coreutils)/bin/md5sum $(BACKENDAIB) > $(BACKENDAIB).md5

$(HABITAT_TFVARS):
	@echo -e "# Generating habitat encryption keys in $(HABITAT_TFVARS)" && \
	  $(SUP_KEY_SH) > $(HABITAT_TFVARS)

## Create both frontend and backend .aib
bundle: $(BUNDLEAIB)

## Create both frontend and backend .aib from dev channel
bundle-dev: $(BUNDLEAIBDEV)

## Create a frontend .aib
fe-bundle: $(FRONTENDAIB)

## Create a backend .aib
be-bundle: $(BACKENDAIB)

## Create Airgap bundles
airgap: bundle

.PHONY: clean help

## Clean up any old manifest and airgap bundles
clean:
	@rm -f $(MANIFESTJSON) $(MANIFEST_TFVARS) $(BACKENDAIB_TFVARS) $(FRONTENDAIB_TFVARS) $(HABITAT_TFVARS) \
		$(TERRAFORM_PATH)/transfer_files/*.aib $(TERRAFORM_PATH)/transfer_files/*.md5

## Deprecated - run ./scripts/smoke-test --help
smoke-test:
	@echo "make smoke-test has been deprecated. Instead, run ./scripts/smoke-test --help"

## Generate random Habitat Supervisor ring key and HTTP gateway bearer token
sup-keys: $(HABITAT_TFVARS) habitat-sup-cert

## Generate habitat certificate for cluster internal secure communication 
habitat-sup-cert:
	bash scripts/cert.sh

## Force a re-push of the .aib files by changing their destination filename.
bump-aib:
	bash scripts/bumpaib.sh $(TERRAFORM_PATH)/terraform.tfvars

## Setup habitat keys and airgap bundles
setup: sup-keys bundle

