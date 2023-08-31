#
# Author:: Faizan Fulara (<ffulara@progress.com>)
# Copyright:: Copyright (c) Chef Software Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
require 'mixlib/config'

module AutomateCluster
  module Config
    VALID_ARCHS = %w{ aws vsphere existing_nodes }.freeze

    class ConfigurationError < StandardError; end

    extend Mixlib::Config

    def self.expand_relative_paths(path)
      return if path.nil?

      # We want to keep the symlink for `/hab/a2_deploy_workspace`
      restore_deploy_symlink File.expand_path(path)
    end

    def self.workspace_symlink
      return nil unless File.exist?('/hab/a2_deploy_workspace')
      File.realpath('/hab/a2_deploy_workspace')
    end

    def self.restore_deploy_symlink(path)
      return path if workspace_symlink.nil?

      path.gsub(/^#{Regexp.escape(workspace_symlink)}/, '/hab/a2_deploy_workspace')
    end

    # Should be one of aws, existing_nodes or vsphere
    default :architecture

    default :ssh_user, 'centos'
    default :ssh_group_name, 'centos'
    default :ssh_port, '22'
    default(:ssh_key_file, '~/.ssh/id_rsa').writes_value { |path| AutomateCluster::Config.expand_relative_paths(path) }
    default(:workspace_path, '/hab/a2_deploy_workspace').writes_value { |path| AutomateCluster::Config.expand_relative_paths(path) }
    default(:secrets_key_file, '/etc/chef-automate/secrets.key').writes_value { |path| AutomateCluster::Config.expand_relative_paths(path) }
    default(:secrets_store_file, 'secrets.json').writes_value { |path| AutomateCluster::Config.expand_relative_paths(path) }
    default :backup_mount, '/mnt/automate_backups'
    default :habitat_uid_gid, ''
    default :automate_dc_token, ''

    config_context :automate do
      default(:admin_password, '').writes_value do |password|
        unless password.length >= 8
          raise ConfigurationError, "Automate admin password must be at least 8 characters long"
        end

        password
      end
      default :fqdn, ''
      default :instance_count, 1
      default :teams_port

      default(:config_file, 'configs/automate.toml').writes_value { |path| AutomateCluster::Config.expand_relative_paths(path) }

      default :enable_custom_certs, false
      default :root_ca
      default :public_key
      default :private_key
      default :certs_by_ip, "{}"
    end

    config_context :chef_server do
      default :instance_count, 1
      default :enable_custom_certs, false
      default :public_key
      default :private_key
      default :certs_by_ip, "{}"
    end

    config_context :opensearch do
      default :instance_count, 3
      default :enable_custom_certs, false
      default :root_ca
      default :admin_key
      default :admin_cert
      default :public_key
      default :private_key
      default :admin_dn
      default :nodes_dn
      default :certs_by_ip, "{}"
    end

    config_context :postgresql do
      default :instance_count, 3
      default :enable_custom_certs, false
      default :root_ca
      default :public_key
      default :private_key
      default :certs_by_ip, "{}"
    end

    # Only applies to existing node architecture
    config_context :existing_nodes do
      default :automate_private_ips, []
      default :chef_server_private_ips, []
      default :opensearch_private_ips, []
      default :postgresql_private_ips, []
    end

    # Only applies to Object storage
    config_context :object_storage do
      default :bucket_name, ""
      default :access_key, ""
      default :secret_key, ""
      default :endpoint, ""
      default :region, ""
      default :location,""
      default :google_service_account_file,"" 
    end

    #Only applies to External Database Configuration
    config_context :managed_aws do
      default :setup_managed_services, false
      default :setup_self_managed_services, false
      default :opensearch_domain_name, ' '
      default :opensearch_domain_url, ' '
      default :opensearch_username, ' '
      default :opensearch_user_password, ' '
      default :aws_os_snapshot_role_arn, ' '
      default :os_snapshot_user_access_key_id, ' '
      default :os_snapshot_user_access_key_secret, ' '
      default :instance_url, ' '
      default :superuser_username, ' '
      default :superuser_password, ' '
      default :dbuser_username, ' '
      default :dbuser_password, ' '
      default :postgresql_root_cert
      default :opensearch_root_cert
    end
    # AWS Related
    config_context :aws do
      default :profile, 'default'
      default :region, 'us-east-1'
      default :vpc_id, " "
      default :cidr_block_addr, " "
      default :private_custom_subnets, []
      default :public_custom_subnets, []
      default :ssh_key_pair_name
      default :lb_access_logs, 'false'
      default :backup_config, ' '
      default :s3_bucketName, 'chef-automate-ha'
      default :setup_managed_services, false
      default :managed_opensearch_domain_name, ' '
      default :managed_opensearch_domain_url, ' '
      default :managed_opensearch_username, ' '
      default :managed_opensearch_user_password, ' '
      default :managed_opensearch_certificate
      default :aws_os_snapshot_role_arn, ' '
      default :os_snapshot_user_access_key_id, ' '
      default :os_snapshot_user_access_key_secret, ' '
      default :managed_rds_instance_url, ' '
      default :managed_rds_superuser_username, ' '
      default :managed_rds_superuser_password, ' '
      default :managed_rds_dbuser_username, ' '
      default :managed_rds_dbuser_password, ' '
      default :managed_rds_certificate
      default :ami_filter_name
      default :ami_filter_virt_type
      default :ami_filter_owner
      default :ami_id
      default :delete_on_termination, true
      default :automate_server_instance_type, 't3a.medium'
      default :chef_server_instance_type, 't3a.medium'
      default :opensearch_server_instance_type, 'm5a.large'
      default :postgresql_server_instance_type, 't3a.medium'
      default :automate_lb_certificate_arn, "arn:aws:acm:...."
      default :chef_server_lb_certificate_arn, "arn:aws:acm:...."
      default :automate_ebs_volume_iops, "100"
      default :automate_ebs_volume_size, "50"
      default :automate_ebs_volume_type, "gp3"
      default :chef_ebs_volume_iops, "100"
      default :chef_ebs_volume_size, "50"
      default :chef_ebs_volume_type, "gp3"
      default :opensearch_ebs_volume_iops, "300"
      default :opensearch_ebs_volume_size, "100"
      default :opensearch_ebs_volume_type, "gp3"
      default :postgresql_ebs_volume_iops, "150"
      default :postgresql_ebs_volume_size, "50"
      default :postgresql_ebs_volume_type, "gp3"

      # tags for AWS infrastructure
      # These individual aws tag configs are deprecated in favor of the tags setting
      default :contact
      default :dept
      default :project

      default :tags do
        {
          "X-Contact" => configuration[:contact],
          "X-Dept" => configuration[:dept],
          "X-Project" => configuration[:project]
        }
      end
    end

    # vSphere Related
    config_context :vsphere do
      default :server, ""
      default :user, ""
      default :password, ""
      default :datacenter, ""
      default :datastore, ""
      default :resource_pool, ""
      default :network, ""
      default :linux_template, ""
    end

    # special config to record which items should be black listed
    # from saving to the config file
    default :blacklist, []
  end
end
