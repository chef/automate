package grpctest

// based on `httptest`, which has copyright:
// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Note (sr): This is super-simple. This doesn't (yet) do any of the
// following things that httptest provides:
// - tls
// - return a client
// - track connection state
// So why is it here, you ask? Well, it chooses a random port, and so far, that
// has been pretty much the only thing we've used httptest for.

import (
	"flag"
	"fmt"
	"net"
	"os"

	"google.golang.org/grpc"
)

// A Server is an HTTP server listening on a system-chosen port on the
// local loopback interface, for use in end-to-end HTTP tests.
type Server struct {
	URL      string // base URL of form ipaddr:port with no trailing slash
	Listener net.Listener

	// Config may be changed after calling NewUnstartedServer and
	// before Start.
	Config *grpc.Server
}

func newLocalListener() net.Listener {
	if *serve != "" {
		l, err := net.Listen("tcp", *serve)
		if err != nil {
			panic(fmt.Sprintf("grpctest: failed to listen on %v: %v", *serve, err))
		}
		return l
	}
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		if l, err = net.Listen("tcp6", "[::1]:0"); err != nil {
			panic(fmt.Sprintf("grpctest: failed to listen on a port: %v", err))
		}
	}
	return l
}

// When debugging a particular grpc server-based test,
// this flag lets you run
//	go test -run=BrokenTest -grpctest.serve=127.0.0.1:8000
// to start the broken server so you can interact with it manually.
var serve = flag.String("grpctest.serve", "", "if non-empty, grpctest.NewServer serves on this address and blocks")

// NewServer starts and returns a new Server.
// The caller should call Close when finished, to shut it down.
func NewServer(s *grpc.Server) *Server {
	ts := NewUnstartedServer(s)
	ts.Start()
	return ts
}

// NewUnstartedServer returns a new Server but doesn't start it.
//
// After changing its configuration, the caller should call Start.
//
// The caller should call Close when finished, to shut it down.
func NewUnstartedServer(s *grpc.Server) *Server {
	return &Server{
		Listener: newLocalListener(),
		Config:   s,
	}
}

// Start starts a server from NewUnstartedServer.
func (s *Server) Start() {
	if s.URL != "" {
		panic("Server already started")
	}
	s.URL = s.Listener.Addr().String()
	s.goServe()
	if *serve != "" {
		fmt.Fprintln(os.Stderr, "grpctest: serving on", s.URL) // nolint: gas
		select {}
	}
}

// Close shuts down the server immediately, closing all connections and
// listeners.
func (s *Server) Close() {
	s.Config.Stop()
}

func (s *Server) goServe() {
	go func() {
		s.Config.Serve(s.Listener) // nolint: errcheck,gas
	}()
}
