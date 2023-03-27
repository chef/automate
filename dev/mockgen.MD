# To generate mock code

// mockgen -destination=components/automate-cli/cmd/chef-automate/mock/mock.go github.com/chef/automate/components/automate-deployment/pkg/client Connection
// mockgen -destination=components/automate-cli/cmd/chef-automate/mock/mock.go github.com/chef/automate/api/interservice/deployment Connection

// mockgen -destination=components/automate-cli/cmd/chef-automate/mock/mock_deployclientstreamer.go github.com/chef/automate/api/interservice/deployment DeployClientStreamer
// mockgen -destination=components/automate-cli/cmd/chef-automate/mock/mock_certificateauthorityserviceclient.go github.com/chef/automate/api/interservice/deployment CertificateAuthorityServiceClient