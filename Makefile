.PHONY: help revendor semgrep semgrep-all

SEMGREP_CONTAINER := returntocorp/semgrep-action:v1
SEMGREP_COMMON_PARAMS := -m semgrep_agent --publish-token ${SEMGREP_TOKEN} --publish-deployment ${SEMGREP_ID}
DOCKER_PARAMS := --volume $(realpath .):/automate --workdir /automate
SEMGREP_MSG := "Need to set SEMGREP_TOKEN and SEMGREP_ID in your env."
SEMGREP_REPO := --env SEMGREP_REPO_NAME=chef/automate
# This is a kludge, using a fictitious repo name to allow using a different semgrep policy.
# (But this causes slack notifications and the semgrep dashboard to have invalid URLs in their links.)
SEMGREP_NIGHTLY_REPO := --env SEMGREP_REPO_NAME=chef/automate-nightly

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

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
