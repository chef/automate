require 'base64'

class SecretKeyBaseGenerator
  # This constant will be used to extract the first `n` characters from the erchef webui private key
  # which will be encrypted to generate the secret_key_base for OCID rails app.
  WEBUI_KEY_SUBSTRING_LEN = 75
  class << self
    # We will save the secret_key_base in a file which will later be used
    # to set it in the OCID environment configuration
    def save_secret_key_base_in_file
      begin
        custom_secret_key_base = ENV['CUSTOM_SECRET_KEY_BASE']

        # If user has provided any custom secret_key_base in automate configuration
        # it will be considered, otherwise we will generate a secret_key_base from the erchef webui_key
        secret_key_base = if (custom_secret_key_base.nil? || custom_secret_key_base.empty?)
          generate_secret_key_base
        else
          custom_secret_key_base
        end
        
        secret_key_base_file_path = ENV['SECRET_KEY_BASE_FILE_PATH']
        
        if File.exists?(secret_key_base_file_path)
          puts "SECRET_KEY_BASE file exists. File will be overwritten.."
        else
          puts "SECRET_KEY_BASE file doesn't exist. It will be generated.."
          dir = File.dirname(secret_key_base_file_path)
          unless File.directory?(dir)
            FileUtils.mkdir_p(dir)
          end
        end
        
        File.write(secret_key_base_file_path, secret_key_base)  
      rescue StandardError => e
        puts "ERROR: Failed to generate secret_key_base file for OCID. Error Details: #{e.inspect}"
        puts "OCID secret_key_base will be assigned a default value..."
      end  
    end

    # Logic to generate a dynamic secret_key_base for OCID
    def generate_secret_key_base
      webui_priv_pem_path = ENV['WEBUI_SRC_PATH']
      secret_key_base = nil
      begin
        webui_priv_key = File.read(webui_priv_pem_path)
        # Get first `WEBUI_KEY_SUBSTRING_LEN` characters
        webui_key_substring_for_encryption = webui_priv_key[0..(WEBUI_KEY_SUBSTRING_LEN-1)]
        secret_key_base = Base64.encode64(webui_key_substring_for_encryption)
      rescue StandardError => e
        puts "Could not find the webui key in erchef service for generation of secret_key_base for OCID. OCID secret_key_base will assume the default value..."
        return nil
      end
      secret_key_base = tidy_up_secret_key_base(secret_key_base)
    end

    # This method is to cleanup any invalid character from the generated `secret_key_base`
    def tidy_up_secret_key_base(secret_key_base)
      # Remove all white space characters
      secret_key_base.gsub!(/\s/, '')
      
      secret_key_base
    end
  end
end

SecretKeyBaseGenerator.save_secret_key_base_in_file
