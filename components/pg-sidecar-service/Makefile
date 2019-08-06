include ../../Makefile.common_go

build:
	@mkdir -p bin
	@go build -o bin/pg-sidecar-service ./cmd/pg-sidecar-service

test:
	@go test ./pkg/...

.DEFAULT_GOAL=build
