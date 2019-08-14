.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

revendor: ## revendor dependencies in vendor/, update .bldr.toml
	dep ensure -v
	go run tools/bldr-config-gen/main.go
