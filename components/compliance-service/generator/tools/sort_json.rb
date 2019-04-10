#!/opt/chefdk/embedded/bin/ruby

require 'json'

# profile = {"controls"=>
#   [{"title"=>"Trusted hosts login",
#     "desc"=>"Rhosts",
#     "impact"=>1.0},
#    {"title"=>"Check owner and permissions for /etc/shadow",
#     "desc"=>"Check periodically the owner and permissions for /etc/shadow",
#     "impact"=>1.0}
#   ],
#  "attributes"=>{"xxx"=>"yyy","aaa"=>"bbb"}}

# will not work for arrays of arrays
def self.sort_hash(h)
  {}.tap do |h2|
    h.sort.each do |k,v|
      if v.is_a?(Hash)
        h2[k] = sort_hash(v)
      else
        if v.is_a?(Array) && v.first.is_a?(Hash)
          h2[k] = v.map{ |vv| sort_hash(vv) }
        else
          h2[k] = v
        end
      end
    end
  end
end

json_file = ARGV.first
json_content = File.read(json_file)
json_hash = JSON.parse(json_content)

puts sort_hash(json_hash).to_json
