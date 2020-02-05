include ../../Makefile.common_go

GOFILES=$(shell GOFLAGS='$(GOFLAGS)' go list ./... | grep -Ev '/vendor|asset\.go|proto|/integration_test' | sed 's/^_//')

default: ci

ci: build lint test

review: lint

# Utilities
build:
	go build cmd/applications-load-gen/applications_load_gen.go

clean:
	@echo "Removing artifacts..."
	rm -f applications-load-gen

## proto:
## 	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component applications-service'

test:
	GOMAXPROCS=4 go test -v -cover $(GOFILES)

# Etc
edit:
	$(EDITOR) Makefile

view:
	$(PAGER) Makefile || cat Makefile

.PHONY: ci review setup clean
.PHONY: build clean fmt lint proto test
.PHONY: edit view
.PHONY: run
.PHONY: generate
