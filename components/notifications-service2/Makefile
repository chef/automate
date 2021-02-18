include ../../Makefile.common_go

GOFILES=$(shell GOFLAGS='$(GOFLAGS)' go list ./... | grep -Ev '/vendor|asset\.go|proto|/integration_test' | sed 's/^_//')

default: ci

ci: build lint test

build:
	go build cmd/notifications-service/notifications-service.go

clean:
	@echo "Removing artifacts..."
	rm -f notifications-service

test:
	GOMAXPROCS=4 go test -v -cover $(GOFILES)

.PHONY: ci review setup clean
.PHONY: build clean fmt lint proto test
.PHONY: edit view
.PHONY: run
.PHONY: generate
