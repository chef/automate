package server

import (
	"context"
	"errors"
	"sync"

	"github.com/chef/automate/api/interservice/keyval"
	"github.com/sirupsen/logrus"
)

type KeyValServer struct {
	mutex sync.Mutex
	vals  map[string]string
}

func New() *KeyValServer {
	return &KeyValServer{
		vals: make(map[string]string),
	}
}

func (s *KeyValServer) PutKey(ctx context.Context, req *keyval.PutKeyRequest) (*keyval.PutKeyResponse, error) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	if req.GetKey() == "" {
		return nil, errors.New("invalid key")
	}

	if req.GetValue() == "" {
		return nil, errors.New("invalid value")
	}

	s.vals[req.GetKey()] = req.GetValue()

	logrus.Infof("Set %q to %q", req.GetKey(), req.GetValue())

	return &keyval.PutKeyResponse{}, nil
}
