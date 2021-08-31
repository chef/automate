.PHONY: help revendor semgrep semgrep-all clean

# Global defaults
TERRAFORM_PATH = $(shell pwd)/terraform
SORT_PY = $(shell pwd)/scripts/tf_var_sort.py
AIRGAP_SH = $(shell pwd)/scripts/bundle_creation.sh
SUP_KEY_SH = $(shell pwd)/scripts/sup-keys.sh
TMPOUT = '/tmp/make.out'
REPO_ROOT = $(shell pwd)
DATESTAMP := $(shell date +"%Y%m%d%H%M%S")
BACKENDAIB = $(TERRAFORM_PATH)/transfer_files/backend-$(DATESTAMP).aib
BACKENDAIB_TFVARS = $(shell pwd)/terraform/a2ha_aib_be.auto.tfvars
FRONTENDAIB = $(TERRAFORM_PATH)/transfer_files/frontend-$(DATESTAMP).aib
FRONTENDAIB_TFVARS = $(shell pwd)/terraform/a2ha_aib_fe.auto.tfvars
TMPFILE := $(shell mktemp)
VARFILES := $(shell find $(TERRAFORM_PATH) -name variables.tf -o -name inputs.tf)
COMPONENTS := $(shell find components -maxdepth 1 -type d -name automate-backend-\* | awk -F/ '{print $2}')
HABITAT_TFVARS = $(shell pwd)/terraform/a2ha_habitat.auto.tfvars
SEMGREP_CONTAINER := returntocorp/semgrep-action:v1
SEMGREP_COMMON_PARAMS := -m semgrep_agent --publish-token ${SEMGREP_TOKEN} --publish-deployment ${SEMGREP_ID}
DOCKER_PARAMS := --volume $(realpath .):/automate --workdir /automate
SEMGREP_MSG := "Need to set SEMGREP_TOKEN and SEMGREP_ID in your env."
SEMGREP_REPO := --env SEMGREP_REPO_NAME=chef/automate
# This is a kludge, using a fictitious repo name to allow using a different semgrep policy.
# (But this causes slack notifications and the semgrep dashboard to have invalid URLs in their links.)
SEMGREP_NIGHTLY_REPO := --env SEMGREP_REPO_NAME=chef/automate-nightly

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
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

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

$(FRONTENDAIB):
	@echo "# Updating FE component package versions: $(FRONTENDAIB_TFVARS)" && \
		$(AIRGAP_SH) -t frontend -o $(FRONTENDAIB) -b $(BACKENDAIB) > $(FRONTENDAIB_TFVARS) && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(FRONTENDAIB) > $(FRONTENDAIB).md5 && \
	  $$(hab pkg path core/coreutils)/bin/md5sum $(BACKENDAIB) > $(BACKENDAIB).md5

$(HABITAT_TFVARS):
	@echo -e "# Generating habitat encryption keys in $(HABITAT_TFVARS)" && \
	  $(SUP_KEY_SH) > $(HABITAT_TFVARS)


## Create a frontend .aib
fe-bundle: $(FRONTENDAIB)

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
setup: sup-keys fe-bundle

revendor: ## revendor dependencies in protovendor/ and update .bldr.toml with deps
	@scripts/revendor.sh

semgrep: ## runs differential semgrep, checking only changes in the current PR, just as is done in Buildkite
	@if [[ -z "${SEMGREP_TOKEN}" || -z "${SEMGREP_ID}" ]]; then echo $(SEMGREP_MSG); else docker run -it --rm --init $(DOCKER_PARAMS) $(SEMGREP_REPO) $(SEMGREP_CONTAINER) python $(SEMGREP_COMMON_PARAMS) --baseline-ref  $(shell git merge-base master HEAD); fi

semgrep-all: ## runs full semgrep but filters out the insignificant issues reported by semgrep-legacy; this is what runs nightly in Buildkite
	@if [[ -z "${SEMGREP_TOKEN}" || -z "${SEMGREP_ID}" ]]; then echo $(SEMGREP_MSG); else docker run -it --rm --init $(DOCKER_PARAMS) $(SEMGREP_NIGHTLY_REPO) $(SEMGREP_CONTAINER) python $(SEMGREP_COMMON_PARAMS); fi

semgrep-legacy: ## runs full semgrep including findings for existing issues that are not significant
	@if [[ -z "${SEMGREP_TOKEN}" || -z "${SEMGREP_ID}" ]]; then echo $(SEMGREP_MSG); else docker run -it --rm --init $(DOCKER_PARAMS) $(SEMGREP_REPO) $(SEMGREP_CONTAINER) python $(SEMGREP_COMMON_PARAMS); fi


# For exploring new rulesets before integrating with CI.
# For convenience, this uses all the ignores from Makefile.common_go and automate-ui/Makefile.
# Ruleset choices come from https://semgrep.dev/explore.
# Example: `make semgrep-test/rc2-bug-scan`
SEMGREP_IGNORE := --exclude third_party --exclude *_test.go --exclude *.pb.go --exclude *.pb.*.go --exclude *.bindata.go --exclude *.spec.ts --exclude coverage --exclude modernizr-custom.js
semgrep-test/%:
	semgrep --config "p/$(@F)" $(SEMGREP_IGNORE)