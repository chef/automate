GOFILES=$(shell go list ./... | grep -Ev '/vendor|asset\.go|proto|/integration_test' | sed 's/^_//')
GOPATH=$(shell go env GOPATH)

include ../../Makefile.common_go

default: ci

ci: build lint vet test

review: lint vet

# Utilities
build:
	go build cmd/event-gateway/event-gateway.go

clean:
	@echo "Removing artifacts..."
	rm -f event-gateway

proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component event-gateway'

test:
	GOMAXPROCS=4 go test -v -cover $(GOFILES)

vet:
	go vet $(GOFILES)

run:
	go run cmd/event-gateway/event-gateway.go serve --config config.dev.toml

# Etc
edit:
	$(EDITOR) Makefile

view:
	$(PAGER) Makefile || cat Makefile

.PHONY: ci review setup clean
.PHONY: build clean fmt lint proto test vet
.PHONY: edit view
.PHONY: run
.PHONY: generate
