#!/bin/bash

go test -race -v -coverprofile=coverage.out ./...
go tool cover -func=coverage.out
go tool cover -html=coverage.out
