#!/bin/bash -ex

export PATH=/opt/chefdk/embedded/bin:$PATH
gem install sinatra

cat << 'EOS' | tee /usr/src/ec2-metadata.rb
#!/usr/bin/env ruby

require "sinatra"

set :port, 9666
set :bind, "0.0.0.0"

# Mock the AWS Marketplace Chef Automate product code for licensing
get "/latest/meta-data/product-codes" do
  "ed3lb0p2oc2ot3v9v72ku1pdt"
end
EOS

ruby /usr/src/ec2-metadata.rb
