GOFILES=$(shell go list ./... | grep -v '/vendor|asset\.go|proto/' | sed 's/^_//')
GOPATH=$(shell go env GOPATH)

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
	GOMAXPROCS=4 go test -v -cover $(GOFILES)

vet:
	go vet $(GOFILES)

run:
	go run cmd/license-control-service/license-control-service.go serve --config config.dev.toml

# Etc
edit:
	$(EDITOR) Makefile

view:
	$(PAGER) Makefile || cat Makefile

.PHONY: ci review setup clean
.PHONY: build clean fmt lint test vet
.PHONY: edit view
.PHONY: run
.PHONY: generate
