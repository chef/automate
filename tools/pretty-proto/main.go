package main

import (
	"fmt"
	"os"

	"github.com/jhump/protoreflect/desc"
	"github.com/jhump/protoreflect/desc/protoparse"
	"github.com/jhump/protoreflect/desc/protoprint"
)

func main() {
	files := os.Args[1:]
	fs := map[string]bool{}
	for _, f := range files {
		fs[f] = true
	}

	parser := protoparse.Parser{
		IncludeSourceCodeInfo: true,
		ImportPaths: []string{
			".",
			"vendor/github.com/grpc-ecosystem/grpc-gateway",                         // protoc-gen-swagger/options/annotations.proto
			"vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis/", // google/api/{annotations,http}.proto
			"vendor/github.com/envoyproxy/protoc-gen-validate/",                     // validate/validate.proto
		},
	}
	printer := protoprint.Printer{
		Compact: true,
	}

	fds, err := parser.ParseFiles(files...)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parser err = %v\n", err)
		os.Exit(1)
	}

	prints := []*desc.FileDescriptor{}
	for _, fd := range fds {
		if fs[fd.GetName()] {
			prints = append(prints, fd)
		}
	}

	err = printer.PrintProtosToFileSystem(prints, ".")
	if err != nil {
		fmt.Fprintf(os.Stderr, "printer err = %v\n", err)
		os.Exit(1)
	}
}
