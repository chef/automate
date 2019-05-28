# DEPRECATED_COMMANDS are commands that were supported in
# automate-ctl but are not supported in the workflow-ctl commands.
DEPRECATED_COMMANDS = [
  { cmd_name: "service-list",
    alternative: "chef-automate service-versions" },
  { cmd_name: "show-config",
    alternative: "chef-automate config show" },
  { cmd_name: "start",
    alternative: "chef-automate start" },
  { cmd_name: "stop",
    alternative: "chef-automate stop" },
  { cmd_name: "restart",
    alternative: "chef-automate restart-services" },
  { cmd_name: "status",
    alternative: "chef-automate status" },
  { cmd_name: "tail",
    alternative: "chef-automate system-logs" },
  { cmd_name: "cleanse",
    alternative: "chef-automate uninstall" },
  { cmd_name: "uninstall",
    alternative: "chef-automate uninstall" },
  { cmd_name: "data-summary" },
  { cmd_name: "node-summary" },
  { cmd_name: "delete-node" },
  { cmd_name: "reconfigure" },
  { cmd_name: "doctor" },
  { cmd_name: "graceful-kill" },
  { cmd_name: "once" },
  { cmd_name: "term" },
  { cmd_name: "hup" },
  { cmd_name: "int" },
  { cmd_name: "kill" },
  { cmd_name: "usr1" },
  { cmd_name: "usr2" },
]

DEPRECATED_COMMANDS.each do |cmd|
  add_command cmd[:cmd_name], "", true do
    log <<-EOF
This command is not supported by workflow-ctl. workflow-ctl only
supports commands relevant to the Workflow component of Chef
Automate.

EOF

    if cmd[:alternative]
      log "Did you mean: #{cmd[:alternative]}"
    else
      log "See\n\n    chef-automate --help\n\nfor more details."
    end
  end
end
