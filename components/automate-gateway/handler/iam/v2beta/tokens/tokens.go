package tokens

import (
	"context"

	"github.com/chef/automate/api/interservice/authn"
	pb_common "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/common"
	pb_req "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/request"
	pb_resp "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/response"
)

// Server is the server interface
type Server struct {
	client authn.TokensMgmtClient
}

// NewServer creates a server with its client.
func NewServer(client authn.TokensMgmtClient) *Server {
	return &Server{client: client}
}

// CreateToken creates a new token.
func (s *Server) CreateToken(
	ctx context.Context, in *pb_req.CreateTokenReq) (*pb_resp.CreateTokenResp, error) {

	// If active value not specified, default to True.
	var active bool
	if in.Active == nil {
		active = true
	} else {
		active = in.Active.GetValue()
	}
	var err error
	var token *authn.Token
	if in.Value != "" {
		req := &authn.CreateTokenWithValueReq{
			Id:          in.Id,
			Description: in.Name,
			Active:      active,
			Value:       in.Value,
			Projects:    in.Projects,
		}
		token, err = s.client.CreateTokenWithValue(ctx, req)
	} else {
		token, err = s.client.CreateToken(ctx, &authn.CreateTokenReq{
			Id:          in.Id,
			Description: in.Name,
			Active:      active,
			Projects:    in.Projects,
		})
	}

	if err != nil {
		return nil, err
	}

	return &pb_resp.CreateTokenResp{Token: convert(token)}, nil
}

// GetToken retrieves a token by id
func (s *Server) GetToken(
	ctx context.Context, in *pb_req.GetTokenReq) (*pb_resp.GetTokenResp, error) {

	token, err := s.client.GetToken(ctx, &authn.GetTokenReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}
	return &pb_resp.GetTokenResp{Token: convert(token)}, nil

}

// UpdateToken uses a token ID to update the Name/Active fields only
func (s *Server) UpdateToken(
	ctx context.Context, req *pb_req.UpdateTokenReq) (*pb_resp.UpdateTokenResp, error) {
	// If active value not specified, default to True.
	var active bool
	if req.Active == nil {
		active = true
	} else {
		active = req.Active.GetValue()
	}

	resp, err := s.client.UpdateToken(ctx, &authn.UpdateTokenReq{
		Id:          req.Id,
		Description: req.Name,
		Active:      active,
		Projects:    req.Projects,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.UpdateTokenResp{Token: convert(resp)}, nil
}

// DeleteToken deletes a token by ID
func (s *Server) DeleteToken(
	ctx context.Context, req *pb_req.DeleteTokenReq) (*pb_resp.DeleteTokenResp, error) {
	_, err := s.client.DeleteToken(ctx, &authn.DeleteTokenReq{Id: req.Id})
	if err != nil {
		return nil, err
	}
	return &pb_resp.DeleteTokenResp{}, nil
}

// ListTokens returns an array of tokens
func (s *Server) ListTokens(
	ctx context.Context, _ *pb_req.ListTokensReq) (*pb_resp.ListTokensResp, error) {
	resp, err := s.client.GetTokens(ctx, &authn.GetTokensReq{})
	if err != nil {
		return nil, err
	}
	tokens := make([]*pb_common.Token, 0, len(resp.Tokens))
	for _, t := range resp.Tokens {
		tokens = append(tokens, convert(t))
	}

	return &pb_resp.ListTokensResp{Tokens: tokens}, nil
}

// Maps the IAMV2 token (name, id) to original token model (id, description).
// Value returned from internal auth service
func convert(token *authn.Token) *pb_common.Token {
	return &pb_common.Token{
		Id:        token.Id,
		Name:      token.Description,
		Active:    token.Active,
		Value:     token.Value,
		CreatedAt: token.Created,
		UpdatedAt: token.Updated,
		Projects:  token.Projects,
	}
}
