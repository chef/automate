package handler

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"

	deployment "github.com/chef/automate/api/interservice/deployment"
	license_control "github.com/chef/automate/api/interservice/license_control"
	"github.com/go-playground/validator/v10"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/external/sso"
	"github.com/golang/protobuf/ptypes/empty"
)

// SsoConfig - the ssoconfig service data structure
type SsoConfig struct {
	license_client license_control.LicenseControlServiceClient
	client         deployment.DeploymentClient
}

type PostConfig struct {
	Ca_contents           string   `json:"ca_contents"  validate:"required"`
	Sso_url               string   `json:"sso_url" validate:"required"`
	Email_attr            string   `json:"email_attr"  validate:"required"`
	Username_attr         string   `json:"username_attr" validate:"required"`
	Groups_attr           string   `json:"groups_attr,omitempty"`
	Allowed_groups        []string `json:"allowed_groups,omitempty"`
	Entity_issuer         string   `json:"entity_issuer"  validate:"required"`
	Name_id_policy_format string   `json:"name_id_policy_format,omitempty"`
}

// NewSsoConfigHandler - create a new ssoconfig service handler
func NewSsoConfigHandler(license_client license_control.LicenseControlServiceClient, client deployment.DeploymentClient) *SsoConfig {
	return &SsoConfig{
		license_client: license_client,
		client:         client,
	}
}
var validate = validator.New()
const ssoFilesPath = "/var/automate-ha/"

func (a *SsoConfig) GetSsoConfig(ctx context.Context, in *empty.Empty) (*sso.GetSsoConfigResponse, error) {

	deploymentType, err := a.getDeploymentDetails(ctx)
	if err != nil {
		return nil, err
	}

	if deploymentType != "SAAS" {
		msg := "Unauthorized: Deployment type is not SAAS"
		return nil, status.Error(7, msg)
	}

	req := &deployment.GetAutomateConfigRequest{}

	res, err := a.client.GetAutomateConfig(ctx, req)
	if err != nil {
		return nil, err
	}

	if res.Config.Dex != nil {
		ssoConfig := res.Config.Dex.V1.Sys.Connectors.Saml

		return &sso.GetSsoConfigResponse{
			CaContents:         ssoConfig.CaContents.GetValue(),
			SsoUrl:             ssoConfig.SsoUrl.GetValue(),
			EmailAttr:          ssoConfig.EmailAttr.GetValue(),
			UsernameAttr:       ssoConfig.UsernameAttr.GetValue(),
			GroupsAttr:         ssoConfig.GroupsAttr.GetValue(),
			AllowedGroups:      ssoConfig.AllowedGroups,
			EntityIssuer:       ssoConfig.EntityIssuer.GetValue(),
			NameIdPolicyFormat: ssoConfig.NameIdPolicyFormat.GetValue(),
		}, nil
	}

	return &sso.GetSsoConfigResponse{
		CaContents:         "",
		SsoUrl:             "",
		EmailAttr:          "",
		UsernameAttr:       "",
		GroupsAttr:         "",
		AllowedGroups:      []string{},
		EntityIssuer:       "",
		NameIdPolicyFormat: "",
	}, nil
}

func (a *SsoConfig) getDeploymentDetails(ctx context.Context) (string, error) {
	deployIDResponse, err := a.license_client.GetDeploymentID(ctx, &license_control.GetDeploymentIDRequest{})
	if err != nil {
		if isServiceDownError(err) {
			log.WithFields(log.Fields{
				"err":  err,
				"func": "getDeploymentDetails",
			}).Error("connecting to the license client")
			return "", nil
		}
		return "", err
	}

	log.Debugf("deployIDResponse.DeploymentType: %s ", deployIDResponse.DeploymentType)

	return deployIDResponse.DeploymentType, nil
}

func (a *SsoConfig) validateDeploymentType(ctx context.Context) error {
	deploymentType, err := a.getDeploymentDetails(ctx)
	if err != nil {
		return err
	}

	if deploymentType != "SAAS" {
		msg := "Unauthorized: Deployment type is not SAAS"
		return status.Error(codes.PermissionDenied, msg)
	}
	return nil
}

func makeRequest(requestType string, url string, jsonData []byte, fileName string) {
	req, err := http.NewRequest(requestType, url, bytes.NewBuffer(jsonData))
	if err != nil {
		log.Fatal("Error occurred", err)
	}

	req.Header.Set("Content-Type", "application/json")
	client := &http.Client{}
	resp, err := client.Do(req)

	if err != nil {
		log.Fatal("Error occurred", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusOK {
		ioutil.WriteFile(ssoFilesPath+fileName, []byte("Success"), 0777)
		return
	}
	ioutil.WriteFile(ssoFilesPath+fileName, []byte("Failure"), 0777)
}

// function to validate ca_contents contains "-----BEGIN CERTIFICATE-----" and "-----END CERTIFICATE-----"
func validateCaContents(ca_contents string) error {
	if strings.Contains(ca_contents, "-----BEGIN CERTIFICATE-----") && strings.Contains(ca_contents, "-----END CERTIFICATE-----") {
		return nil
	} else {
		return errors.New("the ca_contents provided are not correct")
	}
}

func (a *SsoConfig) SetSsoConfig(ctx context.Context, in *sso.SetSsoConfigRequest) (*sso.SetSsoConfigResponse, error) {
	err := a.validateDeploymentType(ctx)
	if err != nil {
		return nil, err
	}
	
	req := &sso.SetSsoConfigRequest{
		CaContents:         in.CaContents,
		SsoUrl:             in.SsoUrl,
		EmailAttr:          in.EmailAttr,
		UsernameAttr:       in.UsernameAttr,
		GroupsAttr:         in.GroupsAttr,
		AllowedGroups:      in.AllowedGroups,
		EntityIssuer:       in.EntityIssuer,
		NameIdPolicyFormat: in.NameIdPolicyFormat,
	}
	
	if err := validateCaContents(req.CaContents); err != nil {
		return nil, err
	}
	
	bodyParams := &PostConfig{
		Ca_contents:           req.CaContents,
		Sso_url:               req.SsoUrl,
		Email_attr:            req.EmailAttr,
		Username_attr:         req.UsernameAttr,
		Groups_attr:           req.GroupsAttr,
		Allowed_groups:        req.AllowedGroups,
		Entity_issuer:         req.EntityIssuer,
		Name_id_policy_format: req.NameIdPolicyFormat,
	}
	err = validate.Struct(bodyParams)
	if err != nil {
		fmt.Print("Unable to fetch the required fields")
		return nil, err
	}
	jsonValue, _ := json.Marshal(bodyParams)
	url, err := getBastionUrl()
	if err != nil {
		log.Fatalln("Errror while creating bastion url = ", err)
		return nil, err
	}
	fileName := "post-status.txt"
	err = ioutil.WriteFile(ssoFilesPath+fileName, []byte("Pending"), 0777)
	if err != nil {
		fmt.Printf("Unable to write the file: %v", err)
	}
	go makeRequest("POST", *url, jsonValue, fileName)
	return &sso.SetSsoConfigResponse{
		Response: "Started the Config Patch",
	}, nil
}

func getBastionUrl() (*string, error) {
	content, err := ioutil.ReadFile(ssoFilesPath + "bastion_info.txt")
	if err != nil {
		log.Fatal("Error occurred while reading file: ", err)
		return nil, err
	}
	url := "http://" + strings.TrimSpace(string(content))
	return &url, nil
}
