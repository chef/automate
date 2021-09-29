package main

var secretsHelpDocs = `
Usage:
    chef-automate secrets SUBCOMMAND [ARG] ...
Parameters:
    SUBCOMMAND                       subcommand
    [ARG] ...                        subcommand arguments
Subcommands:
    init                             Generate a new secrets key used to encrypt the secret store.
    set                              Add a new secrets: for more info use 'secrets set --help'
    show                             Show existing secrets: for more info use 'secrets show --help'
    delete                           Delete existing secrets: for more info use 'secrets delete --help'
Args:
automate_admin_password			Set automate admin password
    sudo_password					Set sudo password to login in machines
    fe_sudo_password				Set different sudo password for frontend machines
    be_sudo_password				Set different sudo password for bacend machines
Options:
    -h, --help                       print help
    -v, --verbose                    Enable verbose output
    -c, --config-file CONFIG_FILE    Load settings from config file (default: "a2ha.rb")
	`
