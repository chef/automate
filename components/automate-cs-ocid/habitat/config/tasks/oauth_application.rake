require 'active_record/base'
require 'fileutils'
namespace :oauth_application do
  desc "Task to register a new oauth application"
  task :register, [:app_name, :redirect_uri] => :environment do |t, args|
    puts "Registering new application..."
    app = Doorkeeper::Application.find_or_create_by(:name => args.app_name)
    app.update(:redirect_uri => args.redirect_uri)
    app.save!
    puts "Registered new application: #{args.app_name} with app: #{app.as_json.to_s}"
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
    
    File.open(file_path, 'w') { |file| file.write(yaml_file_content) }
  end
end
