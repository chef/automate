require_relative 'tfvars_template'
require 'openssl'
require 'tempfile'
require 'fileutils'
require 'json'
require 'tomlrb'

module AutomateCluster
  module Terraform
    module Helpers
      def terraform_path
        File.join(workspace_path, 'terraform')
      end

      def state_file
        @state_file ||= File.join(terraform_path, 'terraform.tfstate')
      end

      def tfvars_file
        @tfvars_file ||= File.join(terraform_path, 'terraform.tfvars')
      end

      def tfvars_template
        @tfvars_template ||= File.join(root_path, 'templates', 'terraform.tfvars.erb')
      end

      def tfarch_file
        @tfarch_file ||= File.join(terraform_path, '.tf_arch')
      end

      def tf_plugins
        @tf_plugins ||= File.join(terraform_path, '.terraform/plugins')
      end

      def state_exists?
        File.exist?(state_file)
      end

      def cluster_configs_exist?
        tf_ready? && tf_hab_secrets_exist?
      end

      def valid_aibs?
        tf_manifest_exist? && tf_be_aib_valid? && tf_fe_aib_valid?
      end

      def tf_hab_secrets_exist?
        config_file = File.join(terraform_path, "a2ha_habitat.auto.tfvars")
        File.exist?(config_file)
      end

      def tf_manifest_exist?
        config_file = File.join(terraform_path, "a2ha_manifest.auto.tfvars")
        File.exist?(config_file)
      end

      def local_aib_exists?(local_file)
        return false unless local_file && !local_file.empty?

        aib_file = File.join(terraform_path, 'transfer_files', local_file)
        File.exist?(aib_file)
      end

      def check_aib_tfvars(tfvar_file, key)
        tfvar = load_aib_tfvars(tfvar_file)
        local_aib_exists? tfvar.fetch(key, false)
      end

      def load_aib_tfvars(tfvar_file)
        return {} unless File.exist?(tfvar_file)
        Tomlrb.load_file(tfvar_file, symbolize_keys: true)
      rescue Tomlrb::ParseError => e
        # Couldn't parse the tfvars file so likely an error happened during the `make airgap` command.
        return {}
      end

      def tf_be_aib_valid?
        config_file = File.join(terraform_path, "a2ha_aib_be.auto.tfvars")
        check_aib_tfvars(config_file, :backend_aib_local_file)
      end

      def tf_fe_aib_valid?
        config_file = File.join(terraform_path, "a2ha_aib_fe.auto.tfvars")
        check_aib_tfvars(config_file, :frontend_aib_local_file)
      end

      def tf_aib_valid?
        config_file = File.join(terraform_path, "a2ha_aib_fe.auto.tfvars")
        check_aib_tfvars(config_file, :frontend_aib_local_file)
      end

      def tf_ready?
        File.exists?(tfarch_file) && File.exists?(tfinit_file)
      end

      def arch_matches?(architecture)
        if File.exists?(tfarch_file)
          existing_arch == architecture
        else
          true
        end
      end

      def existing_arch
        File.read(tfarch_file).chomp if File.exists?(tfarch_file)
      end
    end
  end
end
