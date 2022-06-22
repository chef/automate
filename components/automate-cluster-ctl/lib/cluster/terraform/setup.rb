require_relative 'base'

module AutomateCluster
  module Terraform
    class Setup < Base
      def render_tfvars
        tvfars = AutomateCluster::Terraform::TFVarsTemplate.new(config, tfvars_template)
        output = tvfars.render

        if File.exist?(tfvars_file)
          current_config = OpenSSL::Digest::SHA256.digest(File.read(tfvars_file))
        else
          current_config = ""
        end

        new_config = OpenSSL::Digest::SHA256.digest(output)

        if current_config != new_config
          logger.info "Detected changes in terraform config, generating new terraform.tfvars"
          tmpfile = Tempfile.new(tfvars_file)
          tmpfile << output
          tmpfile.close

          if File.exist?(tfvars_file)
            backup_file = File.join(workspace_path, 'backups', "terraform.tfvars.#{Time.now.strftime('%Y%m%d%H%M%S')}")
            logger.info "Backing up terraform.tfvars to #{backup_file}"
            FileUtils.mkdir_p(File.dirname(backup_file))
            FileUtils.mv(tfvars_file, backup_file)
          end

          FileUtils.mv(tmpfile.path, tfvars_file)
        end
      ensure
        unless tmpfile.nil?
          tmpfile.close
          tmpfile.unlink
        end
      end

      # Add various things we should check for before running terraform commands
      def preflight
        # Automate configs
        unless File.exist? config.automate.config_file
          FileUtils.mkdir_p(File.dirname(config.automate.config_file))
          FileUtils.touch(config.automate.config_file)
        end
      end

      def run(arch)
        preflight
        render_tfvars
        generate_hab_keys

        copy_plans(arch)
        # TODO: Add a step in here to copy the terraform plugin bundles
        run_init(arch)
      end

      def copy_plans(architecture)
        if !arch_matches?(architecture)
          raise UsageError, "Existing arch does not match the requested one (existing: #{existing_arch})"
        end

        wait_while 'Copying terraform plans' do
          shellout!("cp reference_architectures/#{architecture}/* .", cwd: terraform_path)
          shellout!("cp reference_architectures/#{architecture}/.tf_arch .", cwd: terraform_path)
        end
      end

      def generate_hab_keys
        wait_while 'Generating hab-sup keys' do
          run_make_cmd('sup-keys')
        end unless tf_hab_secrets_exist?
      end

      def run_init(architecture)
        # Always run terraform init incase the plans specify a new module to use
        wait_while 'Running terraform init' do
          so = terraform_run('init')
          AutomateCluster.logger.debug so.stdout
          so
        end
      end
    end
  end
end
