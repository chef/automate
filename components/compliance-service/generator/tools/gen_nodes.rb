#!/opt/chefdk/embedded/bin/ruby

require 'json'

i = 0
num = 150
while i < num  do
  puts <<-FOO
- node_name: vqledlsonapdaxxxxxxxxxxxxx#{i}
  environment: DevSec Test Alpha Beta Charlie Delta#{i}
  node_uuid: 1d36bb00-086d-465c-b997-fbb71417c#{i}
  platform_name: d8-2
FOO
  i +=1
end
