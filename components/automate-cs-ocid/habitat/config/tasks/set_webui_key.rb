require 'json'

webui_key = ARGV[0]
private_chef_secrets_path = ARGV[1]

private_chef_secrets_content = File.read(private_chef_secrets_path)
private_chef_secrets_hash = JSON.parse(private_chef_secrets_content)
private_chef_secrets_hash["chef-server"]["webui_key"] = webui_key
File.write(private_chef_secrets_path, JSON.dump(private_chef_secrets_hash))