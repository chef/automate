include ../../Makefile.common_go

default: ci

ci: build

review: lint

# Utilities
build:
	go build cmd/event-service/event-service.go

clean:
	@echo "Removing artifacts..."
	rm -f event-service

proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component event-service'

test:
	GOMAXPROCS=4 go test -v -cover ./...

run:
	go run cmd/event-service/event-service.go serve --config config.dev.toml

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
