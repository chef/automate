package main

var workspaceCommandHelpDocs = `
Usage:
    chef-automate workspace [OPTIONS] SUBCOMMAND [ARG] ...
Parameters:
    SUBCOMMAND                       subcommand
    [ARG] ...                        subcommand arguments
Subcommands:
    setup                            Setup a new workspace
    update                           Update existing workspace
    export                           Export workspace bundle for use in other environments
Options:
    -h, --help                       print help
    -v, --verbose                    Enable verbose output
    -c, --config-file CONFIG_FILE    Load settings from config file (default: "a2ha.rb")
`
