GOFILES=$(shell go list ./... | grep -v '/vendor|asset\.go|proto/' | sed 's/^_//')
GOPATH=$(shell go env GOPATH)

include ../../Makefile.common_go

default: ci

ci: review build

review: lint vet

# Utilities
build:
	go build cmd/data-feed-service/data-feed-service.go

clean:
	@echo "Removing artifacts..."
	rm -f data-feed-service

proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component data-feed-service'

test:
	mkdir .cover
	GOMAXPROCS=4 go test -v -covermode=atomic -coverprofile=.cover/cover.out $(GOFILES)
	go tool cover -html=.cover/cover.out -o .cover/coverage.html

vet:
	go vet $(GOFILES)

run:
	go run cmd/data-feed-service/data-feed-service.go serve --config config.dev.toml

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
