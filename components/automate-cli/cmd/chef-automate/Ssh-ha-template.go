package main

var SshCommandHelpDocs = `
Usage:
    chef-automate [OPTIONS] NAME [COMMAND]

Parameters:
    NAME                             Name of the server type to access, for example automate, chef_server, elasticsearch or postgresql
    [COMMAND]                        Command to run on the remote server

Options:
    -o, --ssh-options OPTIONS        Additional options to pass to ssh
    -l, --list                       List available servers for ssh
    -h, --help                       Print help
    -v, --verbose                    Enable verbose output
    -c, --config-file CONFIG_FILE    Load settings from config file (default: "/src/a2ha.rb")
`
