require 'io/console'
require 'ctl-helpers/prompt'
require 'ctl-helpers/color_printer'

require 'runner/install/options'

module Runner
  module Install
    class Config

      # overrides puts / print / puts_indented / print_warning
      include CtlHelpers::ColorPrinter

      attr_reader :installer, :fqdn, :username, :password, :port, :ssh_identity_file,
                  :enterprise, :chefdk_version, :options, :args, :fips_custom_cert_filename, 
                  :sudo, :client_config

      def initialize(args, description)
        @args = args
        runner_options = Options.new(args, description)
        @option_parser = runner_options.option_parser
        @options = runner_options.options
        @sudo = false
        @client_config = expand_client_config
      rescue OptionParser::MissingArgument,
             OptionParser::InvalidArgument,
             OptionParser::InvalidOption,
             OptionParser::MissingArgument => e
        raise Runner::Exceptions::BadArgumentError.new e.message
      end

      def expand_client_config
        if @options[:full_ohai]
          nil
        else
          "minimal_ohai true\n"
        end
      end

      def needs_sudo!
        @sudo = true
      end

      def confirm_config(node_registration_status)
        unless @yes
          display_config(node_registration_status)
          prompt_for_config
        end
      end

      def display_config(node_registration_status)
        puts_information("\nWelcome to Workflow Runner Installation! " \
                         "We'll have a brand new runner ready in no time.\n")

        puts("First, please take a second to make sure the following configuration " \
             "is correct (you can skip this step by passing -y):\n")
        puts("FQDN: #{@fqdn}")
        puts("Authentication:")
        puts_indented(2, "SSH Username: #{@username}")
        puts_indented(2, "Request sudo: #{sudo}")

        if @port_flag_set
          puts_indented(2, "SSH Port: #{@port} (set via --port PORT)")
        else
          puts_indented(2, "SSH Port: #{@port} (defaulted by not passing --port PORT)")
        end

        if @ssh_identity_file
          puts_indented(2, "SSH Identity filepath: #{@ssh_identity_file} " \
                           "(set via --ssh-identity-file FILEPATH)")

          case
          when !sudo
            # Don't print anything about password when using identity file login
            # and sudo is not required - already printed by installer.rb
          when @password
            puts_indented(2, "Sudo access password: #{redacted} (set via --password PASSWORD)")
          when @password_flag_set
            puts_indented(2, "Sudo access password: WILL PROMPT USER " \
                             "(set via --password with no argument)")
          else
            puts_indented(2, "Sudo access password: PASSWORDLESS SUDO REQUESTED " \
                             "(defaulted by not passing --password or --password PASSWORD)")
          end
        else
          puts_indented(2, "SSH Identity filepath: USING PASSWORD " \
                           "(defaulted by not passing --ssh-identity-file FILEPATH)")
          and_sudo = sudo ? "and Sudo " : ""
          if @password
            puts_indented(2, "SSH #{and_sudo}access password: #{redacted} " \
                             "(set via --password PASSWORD)")

          elsif @password_flag_set
            puts_indented(2, "SSH #{and_sudo}access password: WILL PROMPT USER " \
                             "(set via --password with no argument)")
          end
        end

        if @client_config
          puts "Additional Chef Client configuration:"
          puts_indented(2, @client_config)
        end

        if @installer
          puts("ChefDK: Installing locally from #{@installer} (set via --installer FILEPATH)")
        elsif @chefdk_version
          puts("ChefDK: Downloading custom version #{@chefdk_version} " \
               "(set via --chefdk-version VERSION)")
        else
          puts("ChefDK: Downloading latest stable version of ChefDK " \
               "(defaulted by not passing --chefdk-version or --installer)")
        end

        if @fips_custom_cert_filename
          puts("Self signed certificate: Use #{@fips_custom_cert_filename} as "\
               "the certificate chain instead of a globally known certificate "\
               "to validate runner requests when in FIPS mode. If you are not "\
               "running in FIPS mode or your Automate server has a purchased certificate "\
               "from a known vendor, you do not need to set this option "\
               "(set via --fips-custom-cert-filename CERTIFICATE_FILE)")
        end

        if node_registration_status
          case node_registration_status
          when :client_exists
            print_warning "OVERWRITE: "
            puts("Client exists. Running this installation will overwrite an existing " \
                 "client registered with the chef server named #{@fqdn}.")
          when :node_exists
            print_warning "Overwrite: "
            puts("(Node exists. Running this installation will overwrite an existing node " \
                 "registered with the chef server named #{@fqdn}.")
          end
        end
        puts ""
      end

      def display_usage
        puts @option_parser
      end

      def validate!
        # set arguments
        @fqdn = retrieve_arg(@args, 0, "FQDN")
        @username = retrieve_arg(@args, 1, "USERNAME")
        validate_installer_options

        # set options
        @installer = verify_file!(@options[:installer]) if @options[:installer]
        @chefdk_version = @options[:chefdk_version]
        @ssh_identity_file = verify_file!(@options[:ssh_identity_file]) if @options[:ssh_identity_file]
        @password = @options[:password]
        @password_flag_set = @options[:password_flag_set]
        if @password_flag_set && !@password
          prompt_for_password!
        end
        @port = validate_port!(@options[:port] || 22)
        @port_flag_set = @options[:port_flag_set]
        @enterprise = @options[:enterprise]
        @yes = @options[:yes]
        if @options[:fips_custom_cert_filename]
          exception_object = Runner::Exceptions::CustomCertfileNotFound.new(@options[:fips_custom_cert_filename])
          @fips_custom_cert_filename = verify_file!(@options[:fips_custom_cert_filename],
                                                    exception_object)
        end
      end

      def retrieve_arg(args, i, argname)
        if args[i]
          args[i]
        else
          raise Runner::Exceptions::BadArgumentError.new <<EOF
You failed to pass argument #{i+1} named #{argname}.
EOF
        end
      end

      def validate_installer_options()
        if @options[:installer] && @options[:chefdk_version]
          msg = <<EOF
You cannot specify both --installer and --chefdk-version as they both specify a ChefDK package source.
EOF
          raise Runner::Exceptions::BadArgumentError.new msg
        end

        if !@options[:ssh_identity_file] && !@options[:password_flag_set]
          msg = <<EOF
You did not specify an ssh password or identity file.
Please pass either --password, --ssh-identity-file or both.
EOF
          raise Runner::Exceptions::BadArgumentError.new msg
        end
      end

      def prompt_for_password!
        show_typing = false
        if @ssh_identity_file.nil?
          @password = prompt_user(
            "Password for ssh and sudo access for #{username} on #{fqdn}",
            show_typing
          )
        elsif
          @password = prompt_user(
            "Password for sudo access for #{username} on #{fqdn}",
            show_typing
          )
        end
        puts ""
      end

      def prompt_for_config
        prompt = "Are these the settings you want? (y/n)"
        unless CtlHelpers::Prompt.yes_no_prompt(prompt)
          puts "You chose no. Please re-run the command with the updated settings you want. Exiting..."
          exit 0
        end
        puts ""
      end

      def validate_port!(port)
        min_port = 1
        max_port = 65535
        port_int = port.to_i
        if port_int < min_port || port_int > max_port
          raise Runner::Exceptions::BadArgumentError.new "Error, port '#{port}' is invalid. Expected value between #{min_port} and #{max_port}."
        end
        port_int
      end

      def verify_file!(filename, custom_missing_exception_object=nil)
        missing_exception = if custom_missing_exception_object
                              custom_missing_exception_object
                            else
                              Runner::Exceptions::BadArgumentError.new("Error, file '#{filename}' does not exist. Please verify the path provided.")
                            end

        raise missing_exception unless File.exist?(filename)
        raise Runner::Exceptions::BadArgumentError, "Error, file '#{filename}' is not readable by the current user. Please check permissions on the file, or retry with 'sudo'." unless File.readable?(filename)
        filename
      end

      def prompt_user(prompt, echo = true, default = nil)
        CtlHelpers::Prompt.prompt_user(prompt, echo, default)
      end

      private

      def redacted
        "*"*8
      end

    end
  end
end
