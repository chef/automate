require 'json'
require 'fileutils'

def debug(*args)
  $stderr.puts(*args) if DEBUG
end

DEBUG = ENV.fetch("DEBUG", false)
debug("DEBUG mode on")

# Assume the script is one level deep in /scripts
ROOT_DIR = File.expand_path(File.join(File.basename(__FILE__), ".."))

CONTENT_KEYS = ["definitions", "paths"]

abort("Need input files") if ARGV.length == 0

debug("Using source files:")
debug(ARGV)

primary_file, *swagger_files = ARGV

output = JSON.parse(File.read(primary_file))
output["info"]["title"] = "Combined API docs"

swagger_files.each do |swagger_file|
  swagger_json = JSON.parse(File.read(swagger_file))

  CONTENT_KEYS.each do |key|
    output[key].merge!(swagger_json[key])
  end
end

puts JSON.pretty_generate(output)
