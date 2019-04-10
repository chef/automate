BIN_DIR=$(PWD)/bin
TEST_PACKAGES = ./builder ./notifier

BASE_PROTO_FILES=notifications.proto rules.proto server.proto health.proto

test:
	go test -v $(TEST_PACKAGES)

$(BIN_DIR)/gen_server_test_data:
	go build -o $(BIN_DIR)/gen_server_test_data cmd/gen_server_test_data.go

proto: bin/protoc-gen-go
	protoc -I../../components/notifications-service/server/priv \
		--go_out=plugins=grpc:./api \
		$(BASE_PROTO_FILES)

bin/protoc-gen-go:
	go install -v ../../vendor/github.com/golang/protobuf/protoc-gen-go


clean:
	rm -r ./bin
