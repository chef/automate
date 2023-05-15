require 'yaml'
env_config_file_path = ENV['ENV_CONFIG_FILE_PATH']
puts "env_config_file_path: #{env_config_file_path}"
env_config = YAML.load_file(env_config_file_path)
puts "env_config: #{env_config}"
override_configs = {
  'endpoint' => ENV['CHEF_SERVER_ENDPOINT'],
  'superuser' => ENV['CHEF_SERVER_SUPERUSER'],
  'ssl_verify_mode' => ENV['CHEF_SERVER_SSL_VERIFY_MODE']
}
env_config['chef'] ||= {}
env_config['chef'].merge!(override_configs)
env_config_yml = env_config.to_yaml
puts "yaml type: #{env_config_yml.class.name}"
File.write(env_config_file_path, env_config_yml)
