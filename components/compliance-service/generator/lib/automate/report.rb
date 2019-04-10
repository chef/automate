# encoding: utf-8

require 'net/http'
require 'uri'

class AutomateReporter

  def self.report(dest, dump = nil)
    puts '---> Saving reports to ' + 'Automate (via data-collector)'.red

    headers = { 'Content-Type' => 'application/json' }

    dest['target_opts']['header'].each { | key, value |
      headers[key] = value
    }
    handler = lambda do |node_name, timestamp, report|
      uri = URI(dest['target_opts']['url'])
      req = Net::HTTP::Post.new(uri.path, headers)
      req.body = report.to_json
      begin
        Inspec::Log.debug "Posting report to Chef Automate: #{uri.path}"
        http = Net::HTTP.new(uri.hostname, uri.port)
        if uri.scheme =='https'
          http.verify_mode = OpenSSL::SSL::VERIFY_NONE
          http.use_ssl = true
        end

        http.request(req)
        print '.'
        sleep(0.2)  # Sleep 0.2 seconds between data-collector posts
        return true
      rescue => e
        Inspec::Log.error "send_report: POST to #{uri.path} returned: #{e.message}"
        return false
      end
    end
    handler
  end
end
