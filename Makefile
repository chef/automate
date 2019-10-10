.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# Some of our tests and proto generators require files that go doesn't
# believe in vendoring so we copy them from the module cache to the vendor
# directory ourselves when we revendor.
#
# See https://github.com/golang/go/issues/26366
grpcGatewayModPath=$(shell go list -f {{.Dir}} -m "github.com/grpc-ecosystem/grpc-gateway")
grpcGatewayVendorPath=./vendor/github.com/grpc-ecosystem/grpc-gateway/

.PHONY: revendor
revendor: ## revendor dependencies in vendor/ and update .bldr.toml with deps
	# Revendor
	@go mod tidy
	@go mod vendor
	@go mod verify

	# Clean up files that go mod will vendor that we don't need
	@find ./vendor -type f \( -name .gitignore -o -name .travis.yml -o -name package.json -o -name Makefile -o -name Dockerfile -o -name MAINTAINERS -o -name \*.md -o -name \*.vim -o -name \*.yml \) -delete

	# Add files that go mod won't vendor that we need
	@mkdir -p $(grpcGatewayVendorPath)
	@cp -rf --no-preserve=mode $(grpcGatewayModPath)/* $(grpcGatewayVendorPath)

	# Update .bldr with new dep information
	@go run tools/bldr-config-gen/main.go
