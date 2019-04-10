add_global_pre_hook "ensure that the user is always root" do
  raise "You must run this command as root. Try adding sudo to the start of your command." unless Process.uid == 0 || inside_docker_container? || ARGV.include?('--help') || ARGV.include?('help')
end

## this hook was created to mitigate an incident, and according to the commit message, should only be run on reconfigure. However, reconfigure was difficult to patch from the method itself, and so the prehook was created.
add_global_pre_hook "create /var/opt/delivery/.license.accepted so users don't have to accept it" do
  require 'fileutils'
  if ARGV.include?('reconfigure')
    FileUtils.mkdir_p("/var/opt/delivery")
    FileUtils.touch("/var/opt/delivery/.license.accepted")
  end
end

def reconfigure_post_hook
  require "telemetry/telemetry"
  log ""
  log Telemetry.legal_notice
  log ""
end

def inside_docker_container?
  return false if ENV['TRAVIS'] == 'true' # Builds will hang unless we short-circuit
  file_path = "/proc/1/cgroup"

  if File.exist?(file_path)
    File.read(file_path).include?("docker")
  else
    false
  end
end