require 'json'

RESULT_FILE_NAME = "all-apis.openapi.json"
OPENAPI_DIR = File.join(File.dirname(__FILE__), "..", "data/docs/openapi/")
RESULT_FILE = File.join(OPENAPI_DIR, RESULT_FILE_NAME)
CONTENT_KEYS = ["definitions", "paths"]

openapi_files = Dir["#{OPENAPI_DIR}/*.json"].reject {|f| File.basename(f) == RESULT_FILE_NAME }

output = JSON.parse(File.read(openapi_files.shift))
output["info"]["title"] = RESULT_FILE

openapi_files.each do |openapi_file|
  openapi_json = JSON.parse(File.read(openapi_file))

  CONTENT_KEYS.each do |key|
    output[key].merge!(openapi_json[key])
  end
end

File.write(RESULT_FILE, JSON.dump(output))
