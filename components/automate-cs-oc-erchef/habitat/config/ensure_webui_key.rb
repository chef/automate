if ARGV.empty?
  puts "USAGE: ruby #{$0} DATA_PATH"
  Kernel.exit(1)
end

data_path = ARGV[0]

webui_priv_pem_path = File.join(data_path, "webui_priv.pem")
webui_pub_pem_path = File.join(data_path, "webui_pub.pem")

webui_priv_pem_exist = File.exist?(webui_priv_pem_path)
webui_pub_pem_exist = File.exist?(webui_pub_pem_path)

# If both files exist, we are done
if [ webui_pub_pem_exist, webui_priv_pem_exist ].all?
  $stderr.puts("webui_pub.pem and webui_priv.pem both exist, skipping creation")
  exit 0
end

# If only one of the files exists, something is borked, let the user fix it.
if [ webui_pub_pem_exist, webui_priv_pem_exist ].any?
  $stderr.puts(<<-ERROR_MESSAGE)
One but not both of the webui_pub.pem and webui_priv.pem files exists.
These files have synchronized content and the system will not work if they are
out-of-sync. If you delete both files, they will be regenerated on the next
restart attempt, but any system configured with the webui key will need to be
reconfigured with the new key.

PATHS:
webui_pub.pem:   #{webui_pub_pem_path}
webui_priv.pem: #{webui_priv_pem_path}
ERROR_MESSAGE
  exit 1
end

# Neither file exists, proceed to generate them

# put requires here so we don't load things when not needed
require 'openssl'
require 'json'

private_key = OpenSSL::PKey::RSA.generate(2048)
public_key = private_key.public_key


File.open(webui_priv_pem_path, File::CREAT|File::EXCL|File::NOFOLLOW|File::TRUNC|File::RDWR, 0600) do |f|
  f.print(private_key.to_pem)
end

puts "created webui_priv.pem at #{webui_priv_pem_path}"

File.open(webui_pub_pem_path, File::CREAT|File::EXCL|File::NOFOLLOW|File::TRUNC|File::RDWR, 0600) do |f|
  f.print(public_key.to_pem)
end

puts "created webui_pub.pem at #{webui_pub_pem_path}"
exit 0
