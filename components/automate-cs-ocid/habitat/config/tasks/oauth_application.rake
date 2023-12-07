require 'active_record/base'
require 'fileutils'
namespace :oauth_application do
  desc "Task to register oauth applications"
  task :register => :environment do
    # This task expects an environment variable to be set with a json string
    # containing a list of oauth application objects. The list should have the following schema:
    # [{"name"=>"<app name>", "redirect_uri"=>"<callback url for redirect>"}]
    oauth_apps_config_json = ENV['OAUTH_APPS_CONFIG_JSON']
    oauth_apps_config = JSON.parse(oauth_apps_config_json)
    oauth_applications = oauth_apps_config && oauth_apps_config["oauth_applications"]
    if oauth_applications.present?
      ActiveRecord::Base.transaction do
        # This will make sure a write lock is enabled while running this transaction
        # so that no other write operation can happen on this table during this transaction is in process.
        # Read operations will be allowed even during the lock as it has applied a "SHARE" lock.
        # Only when the current transaction is completed the lock will be revoked and writes will be allowed.
        ActiveRecord::Base.connection.execute('LOCK oauth_applications IN SHARE MODE')
        oauth_applications.each do |oauth_application|
          # Making sure the iteration doesn't execute in case of empty value for app name
          next if oauth_application['name'].blank?
          app = Doorkeeper::Application.find_or_create_by(:name => oauth_application['name'])
          # Updating redirect_uri of the app record only if it's different
          # from the existing value in case it's an existing record
          app.update!(:redirect_uri => oauth_application['redirect_uri']) if (app.redirect_uri != oauth_application['redirect_uri'])
        end
      end
    end
  end

  desc "Task to generate a file with the details of all the registered oauth application under OC-ID"
  task :save_registered_app_details_to_file => :environment do
    registered_apps = Doorkeeper::Application.select(:name, :redirect_uri, :uid, :secret).map(&:attributes).group_by{|app| app["name"]}
    yaml_file_content = registered_apps.to_yaml
    file_path = ENV['REGISTERED_OAUTH_APPS_FILE_PATH']
    begin
      # Making sure the directory is created before writing in the file
      dir = File.dirname(file_path)
      unless File.directory?(dir)
        FileUtils.mkdir_p(dir)
      end
      File.write(file_path, yaml_file_content)  
    rescue StandardError => e
      puts e.inspect
    end
  end
end
