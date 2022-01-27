require_relative 'backend_utils'
require_relative 'certificate_store'
require 'io/console'
require 'thor'
require 'toml-rb'
require 'tempfile'
require 'securerandom'
require 'openssl'

module AutomateCluster
  module Workstation
    class Credentials < Thor
      class CmdFailed < RuntimeError
        def initialize(msg='Command Failed')
          super
        end
      end
      class BadKey < RuntimeError
        def initialize(msg='Invalid Key')
          super
        end
      end

      package_name 'credentials'

      def self.banner(command, namespace = nil, subcommand = false)
        "#{@package_name} #{command.formatted_usage(self, $thor_runner, subcommand)}"
      end

      no_commands do
        def min_length
          10
        end

        def valid_pass(pass)
          pass =~ /^[a-zA-Z0-9\~\!\$\&\(\)_\-\+=,\.]{#{min_length},}$/
        end

        def bad_pass
          puts <<~ENDHEREDOC
            Invalid Characters Detected or Minimum (#{min_length}) Length Requirement not met!
            Only these characters are valid:
              a-z
              A-Z
              0-9
              ~!$&()_-+=,.
          ENDHEREDOC
        end

        def get_pass(role_name)
          if options[:auto]
            pass = SecureRandom.urlsafe_base64(12, true)
            utils.backend_logger.info "Generated password #{pass} for role #{role_name}"
            pass
          else
            loop do
              word = IO.console.getpass "Enter #{role_name} Password: "
              unless valid_pass(word)
                bad_pass
                next
              end
              verify = IO.console.getpass "Enter #{role_name} Password (Verify): "
              return word unless word != verify

              puts 'Passwords do not match!'
              next
            end
          end
        end

        def utils
          @utils ||= BackendUtils::Common.new(options)
        end

        def opts
          # transform Thor options keys to symbols
          options.inject({}) { |memo, (k, v)| memo[k.to_sym] = v; memo }
        end

        def remote_filename
          '/var/tmp/' + SecureRandom.alphanumeric + '.toml'
        end

        def bash_script(cmds)
          <<~ENDHEREDOC
            #!/bin/bash
            set -euo pipefail
            umask 0022
	    export HAB_NONINTERACTIVE=true
	    export HAB_NOCOLORING=true
	    export HAB_LICENSE=accept-no-persist
            [ -s /hab/sup/default/SystemdEnvironmentFile.sh ] && source /hab/sup/default/SystemdEnvironmentFile.sh
            #{cmds}

            # delete ourself
            rm -f -- "$0"
          ENDHEREDOC
        end

        def hab_config_apply(args = {})
          service = args[:service_name]
          leader = utils.pg_hab_elected_leader
          utils.backend_logger.info "Querying Habitat for existing automate-ha-#{service} Gossip Layer Config on #{leader}.."
          script = bash_script <<~SCRIPT
            /bin/hab pkg exec chef/automate-ha-ctl automate-backend-ctl applied --svc=automate-ha-#{service} \
            | grep -v "DEBUG\\|INFO\\|WARN\\|ERROR\\|FATAL" 2>&1
          SCRIPT
          local_script_file = Tempfile.new('script')
          local_script_file.write(script)
          local_script_file.close
          remote_script_filename = remote_filename
          conn = utils.train(leader, opts).connection
          conn.wait_until_ready
          conn.upload(local_script_file.path, remote_script_filename)
          local_script_file.unlink
          utils.backend_logger.debug script
          result = conn.run_command "bash #{remote_script_filename}"
          utils.backend_logger.info "STDOUT: #{result.stdout}\nSTDERR: #{result.stderr}\nEXIT_STATUS: #{result.exit_status}"
          raise CmdFailed, result.stderr if result.exit_status != 0 && options[:exit_on_error]

          existing_gossip_layer_toml = TomlRB.parse(result.stdout.chomp)

          pass_conf = ''
          cert_conf = ''
          if args.key?(:pg_pass) && args[:pg_pass]
            if args[:admin_pass] &&
               args[:replication_pass]
              pass_conf = <<~ENDHEREDOC
                [superuser]
                  password = '#{args[:admin_pass]}'
                [replication]
                  password = '#{args[:replication_pass]}'
              ENDHEREDOC
            else
              utils.backend_logger.error 'PostgreSQL Password values were not set properly - aborting!'
              exit 1
            end
          elsif args.key?(:es_pass) && args[:es_pass]
            if args[:admin_pass] &&
               args[:kibana_pass]
              pass_conf = <<~ENDHEREDOC
                [opendistro_auth]
                  admin_password = '#{args[:admin_pass]}'
                  dashboard_password = '#{args[:kibana_pass]}'
              ENDHEREDOC
            else
              utils.backend_logger.error 'Elasticsearch / Kibana Password values were not set properly - aborting!'
              exit 1
            end
          end

          if args.key?(:pg_certs) && args[:pg_certs]
            if certificates[:postgresql][:private][:value] &&
               certificates[:postgresql][:public][:value] &&
               certificates[:ca_root][:public][:value]
              cert_conf = <<~ENDHEREDOC
                [ssl]
                  enable = true
                  ssl_key = """#{certificates[:postgresql][:private][:value]}"""
                  ssl_cert = """#{certificates[:postgresql][:public][:value]}"""
                  issuer_cert = """#{certificates[:ca_root][:public][:value]}"""
              ENDHEREDOC
            else
              utils.backend_logger.error 'PostgreSQL Certificate values were not set properly - aborting!'
              exit 1
            end
          elsif args.key?(:es_certs) && args[:es_certs]
            if certificates[:elasticsearch][:private][:value] &&
               certificates[:elasticsearch][:public][:value] &&
               certificates[:elasticsearch_admin][:private][:value] &&
               certificates[:elasticsearch_admin][:public][:value] &&
               certificates[:ca_root][:public][:value]
              cert_conf = <<~ENDHEREDOC
                [opendistro_ssl]
                  rootCA = """#{certificates[:ca_root][:public][:value]}"""
                  ssl_cert = """#{certificates[:elasticsearch][:public][:value]}"""
                  ssl_key = """#{certificates[:elasticsearch][:private][:value]}"""
                  admin_cert = """#{certificates[:elasticsearch_admin][:public][:value]}"""
                  admin_key = """#{certificates[:elasticsearch_admin][:private][:value]}"""
                [es_yaml.opendistro_security.authcz]
                  admin_dn = [ "#{certificates[:elasticsearch_admin][:public][:full_cn_reversed]}" ]
                [es_yaml.opendistro_security.ssl.transport]
                  enforce_hostname_verification = false
                  resolve_hostname = false
                [es_yaml.opendistro_security]
                  nodes_dn = [ "#{certificates[:elasticsearch][:public][:full_cn_reversed]}" ]
              ENDHEREDOC
            else
              utils.backend_logger.error 'Elasticsearch Certificate values were not set properly - aborting!'
              exit 1
            end
          elsif args.key?(:kibana_certs) && args[:kibana_certs]
            if certificates[:kibana][:private][:value] &&
               certificates[:kibana][:public][:value] &&
               certificates[:ca_root][:public][:value]
              cert_conf = <<~ENDHEREDOC
                [opendistro_ssl]
                  ssl_key = """#{certificates[:kibana][:private][:value]}"""
                  ssl_cert = """#{certificates[:kibana][:public][:value]}"""
              ENDHEREDOC
            else
              utils.backend_logger.error 'Kibana Certificate values were not set properly - aborting!'
              exit 1
            end
          end

          conf = pass_conf + "\n" + cert_conf
          patch_gossip_layer = TomlRB.parse(conf)
          unified_gossip_layer = existing_gossip_layer_toml.merge(patch_gossip_layer)
          unified_toml = TomlRB.dump(unified_gossip_layer)
          utils.backend_logger.debug \
            "New unified Habitat Gossip Layer Config with changes merged: \n#{unified_toml}"
          local_patch_file = Tempfile.new('patch')
          local_patch_file.write(unified_toml)
          local_patch_file.close
          remote_patch_filename = remote_filename
          conn.upload(local_patch_file.path, remote_patch_filename)
          local_patch_file.unlink
          script = bash_script <<~SCRIPT
            # kill any existing stuck reconfigure hooks
            pkill -9 reconfigure || true
            [ -s #{remote_patch_filename} ] && cat #{remote_patch_filename} | /bin/hab config \
            apply automate-ha-#{service}.default #{Time.now.to_i}
            rm -f #{remote_patch_filename}
          SCRIPT
          local_script_file = Tempfile.new('script')
          local_script_file.write(script)
          local_script_file.close
          remote_script_filename = remote_filename
          conn.upload(local_script_file.path, remote_script_filename)
          local_script_file.unlink
          utils.backend_logger.debug script
          result = conn.run_command "bash #{remote_script_filename}"
          utils.backend_logger.info "STDOUT: #{result.stdout}\nSTDERR: #{result.stderr}\nEXIT_STATUS: #{result.exit_status}"
          raise CmdFailed, result.stderr if result.exit_status != 0 && options[:exit_on_error]
        end

        def patch_frontends(args = {})
          pass_conf = ''
          cert_conf = ''
          if args.key?(:pg_pass) && args[:pg_pass]
            if args[:admin_pass]
              pass_conf += <<~ENDHEREDOC
                [global.v1.external.postgresql.auth.password.superuser]
                  password = '#{args[:admin_pass]}'
                [global.v1.external.postgresql.auth.password.dbuser]
                  password = '#{args[:admin_pass]}'
              ENDHEREDOC
            else
              utils.backend_logger.error 'PG PASS values were not set properly - aborting!'
              exit 1
            end
          end
          if args.key?(:es_pass) && args[:es_pass]
            if args[:admin_pass]
              pass_conf += <<~ENDHEREDOC
                [global.v1.external.elasticsearch.auth]
                  scheme = 'basic_auth'
                [global.v1.external.elasticsearch.auth.basic_auth]
                  password = '#{args[:admin_pass]}'
              ENDHEREDOC
            else
              utils.backend_logger.error 'ES PASS values were not set properly - aborting!'
              exit 1
            end
          end
          if args.key?(:pg_certs) && args[:pg_certs]
            if certificates[:ca_root][:public][:value]
              cert_conf += <<~ENDHEREDOC
                [global.v1.external.postgresql.ssl]
                  enable = true
                  root_cert = """#{certificates[:ca_root][:public][:value]}"""
              ENDHEREDOC
            else
              utils.backend_logger.error 'CA Root values were not set properly - aborting!'
              exit 1
            end
          end
          if args.key?(:es_certs) && args[:es_certs]
            if certificates[:ca_root][:public][:value]
              cert_conf += <<~ENDHEREDOC
                [global.v1.external.elasticsearch.ssl]
                  root_cert = """#{certificates[:ca_root][:public][:value]}"""
                  server_name = "#{certificates[:elasticsearch][:public][:short_cn]}"
              ENDHEREDOC
            else
              utils.backend_logger.error 'CA Root values were not set properly - aborting!'
              exit 1
            end
          end
          if args.key?(:fe_certs) && args[:fe_certs]
            if certificates[:frontend][:public][:value] && certificates[:frontend][:private][:value]
              cert_conf += <<~ENDHEREDOC
                [[load_balancer.v1.sys.frontend_tls]]
                  cert = """#{certificates[:frontend][:public][:value]}"""
                  key = """#{certificates[:frontend][:private][:value]}"""
                [[global.v1.frontend_tls]]
                  cert = """#{certificates[:frontend][:public][:value]}"""
                  key = """#{certificates[:frontend][:private][:value]}"""
              ENDHEREDOC
            else
              utils.backend_logger.error 'Frontend Load Balancer Cert values were not set properly - aborting!'
              exit 1
            end
          end
          conf = pass_conf + "\n" + cert_conf
          local_conf_file = Tempfile.new('conf')
          local_conf_file.write(conf)
          local_conf_file.close

          remote_conf_filename = remote_filename
          # chef-automate config patch will often take but then wait forever if
          # the backends haven't been configured yet. In this case we should
          # power forward and finish the other frontends, then do the backends.
          script = bash_script <<~SCRIPT
            [ -s #{remote_conf_filename} ] && /bin/hab pkg exec core/coreutils timeout 10 /bin/chef-automate config patch #{remote_conf_filename} || true
            [ -s #{remote_conf_filename} ] && /bin/hab pkg exec core/coreutils timeout 10 /bin/chef-automate config show > temp.toml || true
            cp temp.toml /etc/chef-automate/config.toml
            rm -f temp.toml
            rm -f #{remote_conf_filename}
          SCRIPT

          local_script_file = Tempfile.new('script')
          local_script_file.write(script)
          local_script_file.close
          remote_script_filename = remote_filename

          frontends = utils.automate_private_ips + utils.chef_server_private_ips
          frontends.each do |frontend|
            conn = utils.train(frontend, opts).connection
            conn.wait_until_ready
            utils.backend_logger.info "Patching the frontend #{frontend} with: \n#{conf}"
            conn.upload(local_conf_file.path, remote_conf_filename)
            conn.upload(local_script_file.path, remote_script_filename)
            utils.backend_logger.debug script
            result = conn.run_command "bash #{remote_script_filename}"
            utils.backend_logger.info "STDOUT: #{result.stdout}\nSTDERR: #{result.stderr}\nEXIT_STATUS: #{result.exit_status}"
            raise CmdFailed, result.stderr if result.exit_status != 0 && options[:exit_on_error]
          end
          local_conf_file.unlink
          local_script_file.unlink
        end

        def psql_alter_pass(args = {})
          admin_pass = args[:admin_pass]
          replication_pass = args[:replication_pass]
          leader = utils.pg_hab_elected_leader
          utils.backend_logger.info "SQL Altering role passwords on #{leader}.."
          conn = utils.train(leader, opts).connection
          conn.wait_until_ready
          admin_sql = "alter role admin with password '#{admin_pass}'"
          replication_sql = "alter role replication with password '#{replication_pass}'"
          script = bash_script <<~SCRIPT
            PGPASSFILE=/hab/svc/automate-ha-postgresql/config/.pgpass hab pkg exec \
            chef/automate-ha-postgresql psql -U admin -p 5432 -h 127.0.0.1 -d postgres \
            -c "#{replication_sql}" -c "#{admin_sql}"
          SCRIPT
          local_script_file = Tempfile.new('script')
          local_script_file.write(script)
          local_script_file.close
          remote_script_filename = remote_filename

          utils.backend_logger.debug script
          conn.upload(local_script_file.path, remote_script_filename)
          local_script_file.unlink
          result = conn.run_command "bash #{remote_script_filename}"
          utils.backend_logger.info "STDOUT: #{result.stdout}\nSTDERR: #{result.stderr}\nEXIT_STATUS: #{result.exit_status}"
          raise CmdFailed, result.stderr if result.exit_status != 0 && options[:exit_on_error]
        end

        def certificates
          @certificates ||= {
            ca_root: {
              public: {
                filename: "#{utils.top_level_dir}/certs/ca_root.pem",
                value: nil
              }
            },
            postgresql: {
              public: {
                filename: "#{utils.top_level_dir}/certs/pg_ssl_public.pem",
                value: nil
              },
              private: {
                filename: "#{utils.top_level_dir}/certs/pg_ssl_private.key",
                value: nil
              }
            },
            elasticsearch: {
              public: {
                filename: "#{utils.top_level_dir}/certs/es_ssl_public.pem",
                value: nil,
                full_cn_reversed: nil,
                short_cn: nil,
                extendedKeyUsage_client_server: true
              },
              private: {
                filename: "#{utils.top_level_dir}/certs/es_ssl_private.key",
                value: nil
              }
            },
            elasticsearch_admin: {
              public: {
                filename: "#{utils.top_level_dir}/certs/es_admin_ssl_public.pem",
                value: nil,
                full_cn_reversed: nil,
                short_cn: nil,
                extendedKeyUsage_client_server: true
              },
              private: {
                filename: "#{utils.top_level_dir}/certs/es_admin_ssl_private.key",
                value: nil
              }
            },
            kibana: {
              public: {
                filename: "#{utils.top_level_dir}/certs/kibana_ssl_public.pem",
                value: nil
              },
              private: {
                filename: "#{utils.top_level_dir}/certs/kibana_ssl_private.key",
                value: nil
              }
            },
            frontend: {
              public: {
                filename: "#{utils.top_level_dir}/certs/frontend_ssl_public.pem",
                value: nil
              },
              private: {
                filename: "#{utils.top_level_dir}/certs/frontend_ssl_private.key",
                value: nil
              }
            }
          }
        end

        def ssl_prep
          exit_needed = false
          certificates.keys.each do |key|
            if certificates[key].key?(:private)
              path = certificates[key][:private][:filename]
              unless File.file?(path)
                utils.backend_logger.info "Creating SSL skeleton Private Key file: #{certificates[key][:private][:filename]}"
                header = <<~HEADER
                  # This file must contain a RSA / DSA Private Key in PEM format
                  # For example:
                  # -----BEGIN RSA PRIVATE KEY-----
                  # ...
                  # -----END RSA PRIVATE KEY-----
                HEADER
                File.write(path, header)
                exit_needed = true
              end
            end
            path = certificates[key][:public][:filename]
            unless File.file?(path)
              utils.backend_logger.info "Creating SSL skeleton Public Key file: #{certificates[key][:public][:filename]}"
              intermediate = if key == :ca_root
                               nil
                             else
                               <<~HEADER
                                 # If your organization issues from an Intermediate CA,
                                 # append the Intermediate Public Certificate AFTER the Server Certificate.
                               HEADER
                             end
              header = <<~HEADER
                # This file must contain a Public Key in PEM format
                # For example:
                # -----BEGIN CERTIFICATE-----
                # ...
                # -----END CERTIFICATE-----
                #{intermediate}
              HEADER
              File.write(path, header)
              exit_needed = true
            end
          end
          return unless exit_needed

          utils.backend_logger.info 'New files were written out! Stopping for you to populate certs/* as needed.'
          exit 123
        end

        def populate_keys
          utils.backend_logger.info 'Reading CA Root Public Key'
          certstore = CertificateStore.new(utils)

          load_ca_root!(certificates[:ca_root], certstore)

          if options[:pg_ssl] == true || options[:rotate_all] == true
            utils.backend_logger.info 'Reading PostgreSQL keys'
            load_cert!(certificates[:postgresql], certstore)
          end
          if options[:es_ssl] == true || options[:rotate_all] == true
            utils.backend_logger.info 'Reading Elasticsearch Node and Admin keys'
            load_es_cert!(certificates[:elasticsearch], certstore)
            load_es_cert!(certificates[:elasticsearch_admin], certstore)
          end
          if options[:kibana_ssl] == true || options[:rotate_all] == true
            utils.backend_logger.info 'Reading Kibana HTTPS keys'
            load_cert!(certificates[:kibana], certstore)
          end
          if options[:fe_ssl] == true || options[:rotate_all] == true
            utils.backend_logger.info 'Reading Frontend API HTTPS keys'
            load_cert!(certificates[:frontend], certstore)
          end
        end

        def load_ca_root!(info, certstore)
          certstore.load(info[:public][:filename])

          info[:public][:value] = certstore.certificate_output
        end

        def load_cert!(info, certstore)
          certstore.load(info[:public][:filename], info[:private][:filename])

          if !certstore.validator.verify_cert_chain?
            raise BadKey, <<~EOERR
              Unable to validate the certificate chain

              If there are multiple certificates in the file ensure they have been added in the correct order.
              Certificate order: Server Certificate -> Immediate Certificate -> Intermediate Certificate 2

              File: #{certstore.certificate_filename}
              Error: #{certstore.errors.last}
            EOERR
          end

          info[:private][:value] = certstore.key_output
          info[:public][:value] = certstore.certificate_output
        end

        def load_es_cert!(info, certstore)
          load_cert!(info, certstore)

          if !certstore.validator.has_tls_auth?
            raise BadKey, <<~EOERR
              extendedKeyUsage must contain both clientAuth and ServerAuth.

              File: #{certstore.certificate_filename}
              Error: #{certstore.errors.last}
            EOERR
          end

          info[:public][:full_cn_reversed] = certstore.full_cn_reversed
          info[:public][:short_cn] = certstore.short_cn
        end
      end

      # Begin top-level credential set command definitions

      class_option :verbose, type: :boolean, desc: 'Set output to DEBUG level.'
      class_option :sudo, type: :boolean, default: true, desc: 'Run with sudo.'
      class_option :sudo_password, type: :string, lazy_default: -1, desc: 'Specify a sudo password, if it is required.'
      class_option :sudo_options, type: :string, desc: 'Additional sudo options for remote.'
      class_option :sudo_command, type: :string, desc: 'Alternate command for sudo.'
      class_option :exit_on_error, type: :boolean, default: true, desc: 'Exit immediately if a command result has nonzero exit code.'
      class_option :gossip, type: :boolean, default: true, desc: 'Apply the configuration necessary in the Habitat Gossip layer.'
      class_option :frontend, type: :boolean, default: true, desc: 'Chef Automate config patch the frontend(s).'

      desc 'ssl', 'set SSL certificate(s)'
      option :rotate_all, type: :boolean, default: false, desc: 'Rotate all SSL certificates from certs/*'
      option :pg_ssl, type: :boolean, default: false, desc: 'Apply postgres sql SSL from certs/*'
      option :es_ssl, type: :boolean, default: false, desc: 'Apply elasticsearch https SSL from certs/*'
      option :kibana_ssl, type: :boolean, default: false, desc: 'Apply kibana https SSL from certs/*'
      option :fe_ssl, type: :boolean, default: false, desc: 'Apply frontend https SSL from certs/*'
      def ssl
        utils.backend_logger.info '☘  SSL Certificates ☘'

        ssl_prep
        populate_keys

        no_op = true

        if options[:pg_ssl] == true || options[:rotate_all] == true
          no_op = false
          patch_frontends(pg_certs: true) unless options[:frontend] == false
          hab_config_apply(pg_certs: true, service_name: 'postgresql') unless options[:gossip] == false
        end

        if options[:es_ssl] == true || options[:rotate_all] == true
          no_op = false
          patch_frontends(es_certs: true) unless options[:frontend] == false
          hab_config_apply(es_certs: true, service_name: 'elasticsearch') unless options[:gossip] == false
        end

        if options[:kibana_ssl] == true || options[:rotate_all] == true
          no_op = false
          hab_config_apply(kibana_certs: true, service_name: 'kibana')
        end

        if options[:fe_ssl] == true || options[:rotate_all] == true
          no_op = false
          patch_frontends(fe_certs: true)
        end

        unless no_op
          utils.backend_logger.info '★  SSL Certificates Rotated ★'
          return
        end

        utils.backend_logger.error 'You didn\'t specify any Certificates to rotate!'
      rescue BadKey => e
        utils.backend_logger.error e.message
        exit 1
      end

      desc 'postgresql', 'set the PostgreSQL credentials'
      option :auto, type: :boolean, default: true, desc: 'Auto generate new account passwords. Otherwise prompt for values.'
      option :psql, type: :boolean, default: true, desc: 'Alter the role passwords via direct psql.'
      def postgresql
        utils.backend_logger.info '☘  PostgreSQL Credentials ☘'
        admin_pass = get_pass('admin')
        replication_pass = get_pass('replication')
        patch_frontends(pg_pass: true, admin_pass: admin_pass) unless options[:frontend] == false
        psql_alter_pass(admin_pass: admin_pass, replication_pass: replication_pass) unless options[:psql] == false
        hab_config_apply(pg_pass: true, admin_pass: admin_pass, replication_pass: replication_pass, service_name: 'postgresql') unless options[:gossip] == false
        utils.backend_logger.info '★  PostgreSQL Credentials Rotated ★'
      end

      desc 'elasticsearch', 'set the Elasticsearch credentials'
      option :auto, type: :boolean, default: true, desc: 'Auto generate new account passwords. Otherwise prompt for values.'
      def elasticsearch
        utils.backend_logger.info '☘  Elasticsearch Credentials ☘'
        admin_pass = get_pass('admin')
        kibana_pass = get_pass('kibana')
        patch_frontends(es_pass: true, admin_pass: admin_pass) unless options[:frontend] == false
        hab_config_apply(es_pass: true, admin_pass: admin_pass, kibana_pass: kibana_pass, service_name: 'elasticsearch') unless options[:gossip] == false
        utils.backend_logger.info '★  Elasticsearch and Kibana Credentials Rotated ★'
      end

      desc 'all', 'set ALL credentials and ssl certificates'
      def all
        elasticsearch
        postgresql
        ssl
      end
    end

    class CLI < Thor
      desc 'set', 'Set various backend Service Credentials and/or SSL Certificates'
      subcommand 'set', AutomateCluster::Workstation::Credentials
    end
  end
end
