add_command "install-build-node", "Configure the named node to act as a build node in this Delivery Cluster" do
  require 'optparse'
  require 'build-node/installer'
  require 'ctl-helpers/exceptions'

  begin
    log_proc = Proc.new { |output| log output }
    installer = BuildNode::Installer.new(ARGV[1..-1], log_proc)
    installer.configure!
    installer.install
  rescue BuildNode::Exceptions::BadArgumentError => e
    log "\n#{e.message}"
    exit 1
  rescue CtlHelpers::Exceptions::ActionCanceled => e
    log "\nNo action taken, exiting."
  end
end
