require 'active_record/base'
require 'fileutils'
namespace :oauth_application do
  desc "Task to register oauth applications"
  task :register => :environment do
    # This task expects an environment variable to be set with a json string
    # containing a list of oauth application objects. The list should have the following schema:
    # [{"name"=>"<app name>", "redirect_uri"=>"<callback url for redirect>"}]
    oauth_applications_json = ENV['OAUTH_APPLICATIONS_JSON']
    oauth_applications = JSON.parse(oauth_applications_json)
    ActiveRecord::Base.transaction do
      oauth_applications.each do |oauth_application|
        # Making sure the iteration doesn't execute in case of empty value for app name
        next if oauth_application['name'].blank?
        app = Doorkeeper::Application.find_or_create_by(:name => oauth_application['name'])
        app.update!(:redirect_uri => oauth_application['redirect_uri'])
      end
    end
  end

  desc "Task to generate a file with the details of all the registered oauth application under OC-ID"
  task :save_registered_app_details_to_file => :environment do
    registered_apps = Doorkeeper::Application.select(:name, :redirect_uri, :uid, :secret).group_by(&:name).as_json
    yaml_file_content = registered_apps.to_yaml
    file_path = ENV['REGISTERED_OAUTH_APPS_FILE_PATH']

    # Making sure the directory is created before writing in the file
    dir = File.dirname(file_path)
    unless File.directory?(dir)
      FileUtils.mkdir_p(dir)
    end
    
    File.write(file_path, yaml_file_content)
  end
end
