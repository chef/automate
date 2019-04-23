add_command "migrate-github-project", "Migrate a project to new GitHub integration." do
  require "ctl-helpers/erl"
  require "delivery-ctl-helpers/help"

  args = ARGV[1..-1]

  if DeliveryCtlHelpers::Help.help_asked_for? || args.empty?
    puts "Command: workflow-ctl migrate-github-project (ENTERPRISE | ENTERPRISE ORG | ENTERPRISE ORG PROJECT)"
  else
    nag!
    run_command!("#{CtlHelpers::Erl.escript_path}/github_migration #{args.join(' ')}")
  end
end

# we're assuming here that the user is running this command from a TTY (stdin.tty? == true)
# and that this command is not automated.
def nag!
  log('Warning: All cached repos must be cleared out prior to migration.')
  log('See goo.gl/Bm7tPc for instructions).')
  log('Enter "all repos clear" to confirm and proceed.')

  input = STDIN.gets.chomp.downcase
  if input == "all repos clear"
    true
  else
    log('All repos must be cleared. Exiting.')
    Kernel.exit 1
  end
end
