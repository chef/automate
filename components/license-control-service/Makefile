default: ci

ci: build

include ../../Makefile.common_go

review: lint vet

# Utilities
build:
	go build cmd/license-control-service/license-control-service.go

clean:
	@echo "Removing artifacts..."
	@[[ -f license-control-service ]] && rm license-control-service

generate:
	go generate ./...

test:
	go test -cover ./...

run:
	go run cmd/license-control-service/license-control-service.go serve --config config.dev.toml

.PHONY: ci review clean generate test run
