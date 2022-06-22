package main

var provisionInfraHelpDocs = `
Usage:

*** this command will only work with automate HA mode with AWS deployment ***

chef-automate provision-infra <config toml file path> [OPTIONS]

config file path:	
	config toml file path

Options:
    -c, --channel                    Provision infrastructure using particular channel, default channel is current
    -p  --airgap-bundle              Provision using airgap bundle
    -o, --okta-aws                   Use okta_aws cli to authenticate with AWS
    -p, --parallelism WORKERS        Number of workers used for the terraform apply (default: 50)
    -d, --force-deploy               Force terraform deploy even if no changes have been detected. This is used when needing to update the tfstate
    -y, --accept-changes             Automatically accept and apply terraform changes
    -n, --skip-changes               Only show what changes are needed, don't apply them
    -t, --timeout MINUTES            Set timeout value for commands to finish running (default: 10)
    -h, --help                       Print help
    -v, --verbose                    Enable verbose output
    -c, --config-file CONFIG_FILE    Load settings from config file a2ha.rb"
`
