.PHONY: help revendor semgrep semgrep-all

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

revendor: ## revendor dependencies in protovendor/ and update .bldr.toml with deps
	@scripts/revendor.sh

semgrep: ## runs differential semgrep, checking only changes in the current PR, just as is done in CI
	@if [[ -z "${SEMGREP_TOKEN}" || -z "${SEMGREP_ID}" ]]; then echo "Need to set SEMGREP_TOKEN and SEMGREP_ID in your env."; else docker run -it --rm --init --volume $(realpath .):/automate --workdir /automate returntocorp/semgrep-action:v1 python -m semgrep_agent --publish-token ${SEMGREP_TOKEN} --publish-deployment ${SEMGREP_ID} --baseline-ref master; fi

semgrep-all: ## runs full semgrep
	@if [[ -z "${SEMGREP_TOKEN}" || -z "${SEMGREP_ID}" ]]; then echo "Need to set SEMGREP_TOKEN and SEMGREP_ID in your env."; else docker run -it --rm --init --volume $(realpath .):/automate --workdir /automate returntocorp/semgrep-action:v1 python -m semgrep_agent --publish-token ${SEMGREP_TOKEN} --publish-deployment ${SEMGREP_ID}; fi