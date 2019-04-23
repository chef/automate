#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#

require "ctl-helpers/erl"
require "mixlib/shellout"
require "json"
require "ctl-helpers/basic_usage"

# OmnibusCtl#run_command does not exit in case of failure.
# This function uses Mixlib::ShellOut to capture both exit code
# and output, then - in case of a non-zero exit code - explicitly exits
# after displaying any output.
def run_command!(command)
  so = Mixlib::ShellOut.new(command, env: { "ERL_COOKIE" => CtlHelpers::Erl.erl_cookie })
  so.run_command
  if so.status.exitstatus != 0
    puts "Failed to run command: #{@original_command} #{@original_args}"
    # enterprise_ctl (and more generally escript) writes
    # all failure info to stdout:
    puts "Error: #{so.format_for_exception}"
    # Non-zero will raise a SystemExit and stop further execution.
    exit(so.status.exitstatus)
  end
  puts so.stdout
  so.stdout
end

def enterprise_ctl!(cmd_name, args = [])
  @original_command = cmd_name
  @original_args = args.join(" ")
  if ENV["DELIV_DEV"] != nil
    ctl_cmd = "/mnt/sync/delivery/server/apps/enterprise_ctl/enterprise_ctl"
  elsif ENV["ENTERPRISE_CTL_COMMAND"] != nil
    ctl_cmd = ENV["ENTERPRISE_CTL_COMMAND"]
  else
    ctl_cmd = "/opt/delivery/embedded/service/delivery/bin/enterprise_ctl"
  end
  cmd = [ctl_cmd] + cmd_name.split(" ") + args
  run_command!(cmd)
end

# TODO: Stopgap wrapper around add_command to get all enterprise-ctl commands
#       properly responding to help.
#
#       Long term, all arg parsing should probably happen ruby side
#       and the erlang code should simply process work based on
#       already parsed input -- not be a cli.
#       However, flat out crashing when someone passes --help is pretty bad.
def add_enterprise_command(command, description, useless, &block)
  add_command command, description do
    usage = command + " [ARGS] [options..]\n"
    usage += "\nTry passing workflow-ctl #{command} with no arguments for additional details on usage."
    CtlHelpers::BasicUsage.new(ARGV[1..-1], usage, description).parse_help_and_display_usage
    block.call
  end
end

## Enterprise-level commands

add_enterprise_command "create-enterprise",
                       "Create a new enterprise", 2 do
  enterprise_ctl!('create enterprise', ARGV[1..-1])
end

add_enterprise_command "delete-enterprise", "Deletes an existing enterprise", 2 do
  enterprise_ctl!('delete enterprise', ARGV[1..-1])
end

add_enterprise_command "rename-enterprise", "Renames an existing enterprise", 2 do
  enterprise_ctl!('rename enterprise', ARGV[1..-1])
end

add_enterprise_command "list-enterprises", "Lists all enterprises", 2 do
  enterprise_ctl!('list enterprises')
end

## User-level commands

add_enterprise_command "create-user", "Create a new user", 2 do
  enterprise_ctl!('create user', ARGV[1..-1])
end

add_enterprise_command "create-users", "Create new users from a tsv file", 2 do
  enterprise_ctl!('create users', ARGV[1..-1])
end

add_enterprise_command "delete-user", "Deletes an existing user", 2 do
  enterprise_ctl!('delete user', ARGV[1..-1])
end

add_enterprise_command "list-users", "Lists all users", 2 do
  enterprise_ctl!('list users', ARGV[1..-1])
end

add_enterprise_command "reset-password", "Resets the password of a user", 2 do
  enterprise_ctl!('reset password', ARGV[1..-1])
end

add_enterprise_command "generate-password-reset-token", "Generate a password reset token and unset the password for a user", 2 do
  enterprise_ctl!('generate password reset token', ARGV[1..-1])
end

add_enterprise_command "revoke-token", "Revoke's a user's token", 2 do
  enterprise_ctl!('revoke-token', ARGV[1..-1])
end

## Project-level commands
add_enterprise_command "update-project-hooks", "Update git hooks for all projects.", 2 do
  enterprise_ctl!('update-project-hooks')
end

add_enterprise_command "delete-project", "Deletes an existing project", 2 do
  enterprise_ctl!('delete project', ARGV[1..-1])
end

## Migrations

## This is a short lived migration call for updating the deliv_change db records
## to retroactively have a title and description generated from their commit
## history.
##
## WARNING: Only use this if you know what you are doing. Consider a dry
## run first.

add_enterprise_command "migrate-change-description", "Migrate change description LIVE RUN", 2 do
  enterprise_ctl!('migrate-change-description')
end

## Same as above, but performs a dry run instead. There are no write actions,
## but there are read actions, which could in theory have side effects, though
## they should be neglible.
##
## WARNING: You should always make a backup first!
add_enterprise_command "migrate-change-description-dry-run", "Migrate change description DRY RUN", 2 do
  enterprise_ctl!('migrate-change-description-dry-run')
end

add_enterprise_command "migrate-patchset-diffs", "Update patchset diffs to include numstat", 2 do
  enterprise_ctl!('migrate-patchset-diffs')
end

add_enterprise_command "migrate-patchset-diffs-dry-run", "Update patchset diffs to include numstat DRY RUN", 2 do
  enterprise_ctl!('migrate-patchset-diffs-dry-run')
end
