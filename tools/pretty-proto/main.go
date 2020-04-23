package main

import (
	"fmt"
	"os"

	"github.com/jhump/protoreflect/desc/protoparse"
	"github.com/jhump/protoreflect/desc/protoprint"
)

func main() {
	parser := protoparse.Parser{
		ImportPaths: []string{
			".",
			"vendor/github.com/grpc-ecosystem/grpc-gateway",                         // protoc-gen-swagger/options/annotations.proto
			"vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis/", // google/api/{annotations,http}.proto
			"vendor/github.com/envoyproxy/protoc-gen-validate/",                     // validate/validate.proto
		},
	}
	printer := protoprint.Printer{}

	fs, err := parser.ParseFiles(os.Args[1:]...)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parser err = %v\n", err)
		os.Exit(1)
	}

	// NB: file descriptors are sorted topologically by imports, so
	//     the last one is the one we've passed
	f := fs[len(fs)-1:]

	err = printer.PrintProtosToFileSystem(f, ".")
	if err != nil {
		fmt.Fprintf(os.Stderr, "printer err = %v\n", err)
		os.Exit(1)
	}
}
