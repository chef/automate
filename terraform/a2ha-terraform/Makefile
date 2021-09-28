# Global defaults
TERRAFORM_PATH = $(shell pwd)
SORT_PY = $(shell pwd)/../../scripts/tf_var_sort.py
AIRGAP_SH = $(shell pwd)/../../scripts/bundle_creation.sh
SUP_KEY_SH = $(shell pwd)/../../scripts/sup-keys.sh
TMPOUT = '/tmp/make.out'
REPO_ROOT = $(shell pwd)
DATESTAMP := $(shell date +"%Y%m%d%H%M%S")
BUNDLEAIB = $(TERRAFORM_PATH)/transfer_files/bundle-$(DATESTAMP).aib
BACKENDAIB = $(TERRAFORM_PATH)/transfer_files/backend-$(DATESTAMP).aib
BACKENDAIB_TFVARS = $(shell pwd)/a2ha_aib_be.auto.tfvars
FRONTENDAIB = $(TERRAFORM_PATH)/transfer_files/frontend-$(DATESTAMP).aib
FRONTENDAIB_TFVARS = $(shell pwd)/a2ha_aib_fe.auto.tfvars
TMPFILE := $(shell mktemp)
VARFILES := $(shell find $(TERRAFORM_PATH) -name variables.tf -o -name inputs.tf)
HABITAT_TFVARS = $(shell pwd)/a2ha_habitat.auto.tfvars

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

## Canonical style and lint checks
check: tf-cp-aws tf-init canonical-check validate-check sort-check exampletfvars-check # hab-shellcheck tf-shellcheck

## Format TF files into Canonical style
fmt: sort canonical canonical-check

canonical-check:
	terraform fmt -check=true -list=true -diff=true $(TERRAFORM_PATH)

hab-shellcheck:
	shellcheck -e SC2034,SC2154,SC2148 **/**/**/*.sh

#$(TERRAFORM_PATH)/main.tf $(TERRAFORM_PATH)/variables.tf $(TERRAFORM_PATH)/outputs.tf
tf-cp-aws:
	> $(TMPOUT); \
	  cp $(TERRAFORM_PATH)/reference_architectures/aws/*.tf $(TERRAFORM_PATH)/

tf-init:
	> $(TMPOUT); \
	terraform init $(TERRAFORM_PATH)

sort-check:
	> $(TMPOUT); \
	  for file in $(VARFILES); \
	  do \
	    python3 $(SORT_PY) $$file > $(TMPFILE); \
	    diff -bq $(TMPFILE) $$file > /dev/null 2>&1 || echo $$file >> $(TMPOUT); \
	  done;
	@[ ! -s $(TMPOUT) ] || \
	  { echo '✘ The following files are unsorted:'; cat $(TMPOUT); echo; exit 5; }
	@echo sort-check complete ✔

exampletfvars-check:
	> $(TMPOUT); \
	  grep _svc_ terraform/terraform.tfvars.example | awk '{print $$1}' | sort -u > /tmp/tfvars.example; \
	  grep -r _svc_channel terraform/**/**/inputs.tf | awk '{print $$2}' | sed -e 's/"//g' | sort -u > /tmp/inputs.tf; \
	  diff -b /tmp/tfvars.example /tmp/inputs.tf > $(TMPOUT) || true;
	@[ ! -s $(TMPOUT) ] || \
	  { echo '✘ The following differences exist between:'; echo '< terraform/terraform.tfvars.example'; echo '> terraform/**/**/inputs.tf'; cat $(TMPOUT); echo; exit 5; }
	@echo exampletfvars-check complete ✔

tf-shellcheck:
	shellcheck -e SC2034,SC2154,SC2148 **/**/**/**/*.sh.tpl

validate-check:
	terraform validate $(TERRAFORM_PATH)

canonical:
	terraform fmt -list=true -diff=true $(TERRAFORM_PATH)

sort:
	@which python3 >/dev/null 2>&1 || (echo "No python3! $$?"; exit 1)
	for file in $(VARFILES); \
	  do \
	    $(SORT_PY) $$file > $(TMPFILE); \
	    diff -bq $(TMPFILE) $$file > /dev/null 2>&1 || echo "sorted: $$file"; \
	    cp $(TMPFILE) $$file; \
	  done;

$(BUNDLEAIB):
	@echo "# Updating FE and BE component package versions" && \
		$(AIRGAP_SH) -t all -o $(FRONTENDAIB) -b $(BACKENDAIB) -v $(FRONTENDAIB_TFVARS) -q $(BACKENDAIB_TFVARS)  && \
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
sup-keys: $(HABITAT_TFVARS)

## Force a re-push of the .aib files by changing their destination filename.
bump-aib:
	bash scripts/bumpaib.sh $(TERRAFORM_PATH)/terraform.tfvars

## Setup habitat keys and airgap bundles
setup: sup-keys bundle
