package server

import (
	"context"
	"encoding/json"
	"net/http"
	"net/url"

	chef "github.com/go-chef/chef"
	jsonpb "github.com/golang/protobuf/jsonpb"
	structpb "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/components/infra-proxy-service/service"
)

// ChefConfig is an infra-proxy server
type ChefConfig struct {
	Name    string
	Key     string
	SkipSSL bool
	BaseURL string
}

// ChefClient type definition for the chef client
type ChefClient struct {
	client *chef.Client
}

// SearchObjectsWithDefaults constructs the search query with defaults params values
// Default per_page is set 1000
// Default search term q is "*:*"
// Default page is set to 0
func (c *ChefClient) SearchObjectsWithDefaults(searchIndex string, searchQuery *request.SearchQuery, params map[string]interface{}) (*chef.SearchResult, error) {
	var result chef.SearchResult
	perPage := int(searchQuery.GetPerPage())
	if perPage == 0 {
		perPage = 1000
	}

	searchStr := searchQuery.GetQ()
	if searchStr == "" {
		searchStr = "*:*"
	}
	query, err := c.client.Search.NewQuery(searchIndex, searchStr)
	if err != nil {
		return nil, ParseAPIError(err)
	}
	query.Rows = perPage

	// Query accepts start param, The row at which return results begin.
	query.Start = int(searchQuery.GetPage()) * perPage

	if params == nil {
		result, err = query.Do(c.client)
		if err != nil {
			return nil, ParseAPIError(err)
		}
	} else {
		result, err = query.DoPartial(c.client, params)
		if err != nil {
			return nil, ParseAPIError(err)
		}
	}

	// Chef infra Server search API returning starting page like, starting of records offset value
	// converting it to page value with respect to per page.
	if result.Start != 0 {
		result.Start = result.Start / perPage
	}

	return &result, nil
}

// NewChefClient is an infra-proxy server
func NewChefClient(config *ChefConfig) (*ChefClient, error) {

	// build a client
	client, err := chef.NewClient(&chef.Config{
		Name:    config.Name,
		Key:     config.Key,
		SkipSSL: config.SkipSSL,
		BaseURL: config.BaseURL,
	})

	if err != nil {
		return nil, errors.Errorf("The user or client who made the request could not be authenticated. Verify the user/client name, and that the correct key was used to sign the request.")
	}

	return &ChefClient{client: client}, nil
}

func (s *Server) createClient(ctx context.Context, orgID string, serverID string) (*ChefClient, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// TODO: combine get server & org query in one statement.
	server, err := s.service.Storage.GetServer(ctx, serverID)
	if err != nil {
		return nil, service.ParseStorageError(err, serverID, "server")
	}

	org, err := s.service.Storage.GetOrg(ctx, orgID, serverID)
	if err != nil {
		return nil, service.ParseStorageError(err, orgID, "org")
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: org.CredentialID})
	if err != nil {
		return nil, err
	}

	baseURL, err := targetURL(server.Fqdn, server.IPAddress, org.Name)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid server url: %s", baseURL)
	}

	client, err := NewChefClient(&ChefConfig{
		Name:    org.AdminUser,
		Key:     GetOrgAdminKeyFrom(secret),
		SkipSSL: true,
		BaseURL: baseURL,
	})

	return client, err
}

// targetURL is constructing the base URL based on fqdn|ipAddress value
func targetURL(fqdn string, IPAddress string, orgName string) (string, error) {
	path := fqdn
	if path == "" {
		path = IPAddress
	}
	path = path + "/organizations/" + orgName + "/"

	baseURL, err := url.Parse(path)
	if err != nil {
		return "", errors.Wrap(err, err.Error())
	}
	baseURL.Scheme = "https"

	return baseURL.String(), nil
}

// GetOrgAdminKeyFrom returns AdminKey
func GetOrgAdminKeyFrom(secret *secrets.Secret) string {
	adminKey := ""
	if secret != nil {
		for _, item := range secret.Data {
			if item.Key == "key" {
				adminKey = item.Value
			}
		}
	}

	return adminKey
}

// ParseAPIError parses common Chef Infra Server API errors into a user-readable format.
func ParseAPIError(err error) error {
	chefError, _ := chef.ChefError(err)
	if chefError != nil {
		switch chefError.StatusCode() {
		case http.StatusBadRequest:
			return status.Errorf(codes.InvalidArgument, chefError.StatusMsg())
		case http.StatusUnauthorized:
			return status.Errorf(codes.Unauthenticated, chefError.StatusMsg())
		case http.StatusForbidden:
			return status.Errorf(codes.PermissionDenied, chefError.StatusMsg())
		case http.StatusConflict:
			return status.Errorf(codes.AlreadyExists, chefError.StatusMsg())
		case http.StatusNotFound:
			return status.Errorf(codes.NotFound, chefError.StatusMsg())
		default:
			return status.Error(codes.InvalidArgument, chefError.StatusMsg())
		}
	}
	return err
}

// StructToJSON convert the structpb to JSON interface object.
func StructToJSON(data *structpb.Struct) (interface{}, error) {
	if data == nil {
		data = &structpb.Struct{}
	}

	jsonStr, err := (&jsonpb.Marshaler{}).MarshalToString(data)
	if err != nil {
		return nil, err
	}

	var v interface{}
	err = json.Unmarshal(json.RawMessage(jsonStr), &v)

	return v, err
}
