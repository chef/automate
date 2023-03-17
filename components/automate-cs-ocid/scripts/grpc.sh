protoc -I .\
    -I "$GOPATH/src" \
    --go_out=plugins=grpc:"$GOPATH/src" \
    --a2-config_out=$GOPATH/src \
    "${proto_files}"