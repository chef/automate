package handler

import (
	"context"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/golang/protobuf/proto"
)

// Secrets - the secrets service data structure
type Secrets struct {
	client secrets.SecretsServiceClient
}

// NewSecretsHandler - create a new secrets service handler
func NewSecretsHandler(secretClient secrets.SecretsServiceClient) *Secrets {
	return &Secrets{
		client: secretClient,
	}
}

// Create - create a new secret
func (a *Secrets) Create(ctx context.Context, in *secrets.Secret) (*secrets.Id, error) {
	inDomain := &secrets.Secret{}
	out := &secrets.Id{}
	f := func() (proto.Message, error) {
		return a.client.Create(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// Read - read the stored information for one secret.
func (a *Secrets) Read(ctx context.Context, in *secrets.Id) (*secrets.Secret, error) {
	inDomain := &secrets.Id{}
	out := &secrets.Secret{}
	f := func() (proto.Message, error) {
		return a.client.Read(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	if key, found := findKeyValue(out.Data, "password"); found {
		key.Value = ""
	}
	if key, found := findKeyValue(out.Data, "sudo_password"); found {
		key.Value = ""
	}
	if key, found := findKeyValue(out.Data, "key"); found {
		key.Value = ""
	}
	return out, nil
}

// Update - update an existing secret
func (a *Secrets) Update(ctx context.Context, in *secrets.Secret) (*secrets.UpdateResponse, error) {
	inDomain := &secrets.Secret{}
	out := &secrets.UpdateResponse{}
	f := func() (proto.Message, error) {
		return a.client.Update(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// Delete - delete an existing secret
func (a *Secrets) Delete(ctx context.Context, in *secrets.Id) (*secrets.DeleteResponse, error) {
	inDomain := &secrets.Id{}
	out := &secrets.DeleteResponse{}
	f := func() (proto.Message, error) {
		return a.client.Delete(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// List - List the stored secret based off the provided query
func (a *Secrets) List(ctx context.Context, in *secrets.Query) (*secrets.Secrets, error) {
	inDomain := &secrets.Query{}
	out := &secrets.Secrets{}
	f := func() (proto.Message, error) {
		return a.client.List(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func findKeyValue(items []*secrets.Kv, key string) (*secrets.Kv, bool) {
	for _, item := range items {
		if item.Key == key {
			return item, true
		}
	}
	return nil, false
}
