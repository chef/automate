package handler

import (
	"context"

	// upstream definitions
	"github.com/chef/automate/api/interservice/authn"

	authReq "github.com/chef/automate/components/automate-gateway/api/auth/tokens/request"
	authRes "github.com/chef/automate/components/automate-gateway/api/auth/tokens/response"
)

// TokensServer stores token
type TokensServer struct {
	client authn.TokensMgmtClient
}

// NewTokensMgmtServer initializes Server with client
func NewTokensMgmtServer(tokensMgmtClient authn.TokensMgmtClient) *TokensServer {
	return &TokensServer{
		client: tokensMgmtClient,
	}
}

func (a *TokensServer) CreateToken(ctx context.Context, r *authReq.CreateToken) (*authRes.Token, error) {
	// If a token value is provided, create the token using that value.
	if r.GetValue() != "" {
		req := &authn.CreateTokenWithValueReq{
			Id:          r.GetId(),
			Active:      r.GetActive(),
			Description: r.GetDescription(),
			Value:       r.GetValue(),
		}
		res, err := a.client.CreateTokenWithValue(ctx, req)
		return fromUpstreamToken(res), err
	}

	req := &authn.CreateTokenReq{
		Id:          r.GetId(),
		Active:      r.GetActive(),
		Description: r.GetDescription(),
	}
	res, err := a.client.CreateToken(ctx, req)
	return fromUpstreamToken(res), err
}

func (a *TokensServer) UpdateToken(ctx context.Context, r *authReq.UpdateToken) (*authRes.Token, error) {
	req := &authn.UpdateTokenReq{
		Id:          r.GetId(),
		Active:      r.GetActive(),
		Description: r.GetDescription(),
	}
	res, err := a.client.UpdateToken(ctx, req)
	return fromUpstreamToken(res), err
}

func (a *TokensServer) DeleteToken(ctx context.Context, r *authReq.Uuid) (*authRes.DeleteTokenResp, error) {
	_, err := a.client.DeleteToken(ctx, &authn.DeleteTokenReq{Id: r.GetId()})

	return &authRes.DeleteTokenResp{}, err
}

func (a *TokensServer) GetToken(ctx context.Context, r *authReq.Uuid) (*authRes.Token, error) {
	res, err := a.client.GetToken(ctx, &authn.GetTokenReq{Id: r.GetId()})
	return fromUpstreamToken(res), err
}

func (a *TokensServer) GetTokens(ctx context.Context, _ *authReq.GetTokensReq) (*authRes.Tokens, error) {
	res, err := a.client.GetTokens(ctx, &authn.GetTokensReq{})
	if err != nil {
		return nil, err
	}
	cs := []*authRes.Token{}
	for _, c := range res.Tokens {
		cs = append(cs, fromUpstreamToken(c))
	}
	return &authRes.Tokens{Tokens: cs}, nil
}

func fromUpstreamToken(c *authn.Token) *authRes.Token {
	if c == nil {
		return nil
	}

	return &authRes.Token{
		Id:          c.Id,
		Description: c.Description,
		Value:       c.Value,
		Active:      c.Active,
		Created:     c.Created,
		Updated:     c.Updated,
	}
}
