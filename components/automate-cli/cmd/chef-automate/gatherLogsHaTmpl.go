package main

var gatherLogsHelpDoc = `
Usage:
    chef-automate gather-logs
Options:
    -o, --ssh-options OPTIONS          Additional options to pass to ssh
    -c, --sudo-command SUDO_COMMAND    Alternate command for sudo (default: "sudo")
    -s, --sudo-options SUDO_OPTIONS    Additional options for remote sudo
    -r, --remote-log-dir DIRECTORY     Where to temporarily store remote log files (default: "/var/tmp")
    -l, --local-log-dir DIRECTORY      Where to store the logs locally (default: "/var/tmp")
    -n, --log-lines NUMBER             How many journalctl lines to capture
    -h, --help                         Print help
    -v, --verbose                      Enable verbose output
    -c, --config-file CONFIG_FILE      Load settings from config file (default: "config.toml")
`
