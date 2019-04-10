
##### TESTING
ruby-grpc-tools:
	@if ! gem list grpc -i; then gem install grpc; fi > /dev/null
	@if ! gem list grpc-tools -i; then gem install grpc-tools; fi > /dev/null

PROTOS = $(shell find ../../api/external/secrets -name '*.proto')
RUBY_PROTOS = $(patsubst %.proto, %_pb.rb, $(PROTOS))

generate-ruby-grpc: ruby-grpc-tools $(RUBY_PROTOS)

%_pb.rb : %.proto
	@echo " * Compiling test proto for $<"
	grpc_tools_ruby_protoc \
		-I../.. \
		-I$(GOPATH)/src \
		-I../../vendor \
		-I../../vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
		--ruby_out=../.. \
		--grpc_out=../.. \
		$<


proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component secrets-service'
