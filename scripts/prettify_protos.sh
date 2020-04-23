#!/bin/bash

find api components -name '*.proto' -exec go run tools/pretty-proto/main.go {} \;
