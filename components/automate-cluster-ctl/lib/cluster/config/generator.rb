require 'pp'

module AutomateCluster
  class ConfigGenerator
    SECTION_COMMENT = {
      aws: "Only applies when using AWS architecture",
      vsphere: "Only applies when using vSphere architecture",
      existing_nodes: "Only applies when using an existing node architecture",
      automate: "Automate frontend node related settings",
      postgresql: "PostgreSQL related settings",
      elasticsearch: "Elasticsearch related settings",
      chef_server: "Chef Server frontend node related settings",
    }.freeze

    SETTING_COMMENT = {
      sudo_password: "Use `chef-automate-ctl secrets set sudo_password` instead",
      aws: {
        profile: "AWS Credentials profile to use when deploying AWS infrastructure",
        contact: "DEPRECATED: AWS Tag: Contact email to apply to AWS insfrastructure tags",
        ami_filter_name: "Filter settings default to CentOS if left blank",
        ami_filter_owner: "Filter settings default to CentOS if left blank",
        ami_filter_virt_type: "Filter settings default to CentOS if left blank",
        ami_id: "Overrides ami filter search features",
        lb_access_logs: "If lb_access logs is true then elb logs will be enabled and new s3 bucket will be created as name of a2ha-elb-bucket. Please find it after provision in your aws console",
        s3_bucketName: "The name of s3 bucket to be created for taking backup , by default name will be chef-automate-ha",
        dept: "DEPRECATED: AWS Tag: Department name to apply to AWS insfrastructure tags",
        project: "DEPRECATED: AWS Tag: Project name to apply to AWS insfrastructure tags",
        automate_lb_certificate_arn: "AWS Certificate is specific to the region and AWS account this is being deployed to.",
        chef_server_lb_certificate_arn: "AWS Certificate is specific to the region and AWS account this is being deployed to.",
        automate_server_instance_type: "EC2 instance type to use for Automate frontends, minimum >2G of RAM for test, 8G for prod",
        chef_server_instance_type: "EC2 instance type to use for Chef Server frontends, minimum >2G of RAM for test, 8G for prod",
        elasticsearch_server_instance_type: "EC2 instance type to use for Elasticsearch backends, minimum 8G of RAM for test, 16G for prod",
        postgresql_server_instance_type: "EC2 instance type to use for PostgreSQL backends, minimum 4G of RAM for test, 8G for prod",
      },
      automate: {
        fqdn: "Leave commented out if using AWS infrastructure",
        teams_port: ["Uncomment and set this value if the teams service",
                     "port (default: 10128) conflicts with another service."]
      }
    }.freeze

    SKIP_SECTIONS = {
      aws: [ :existing_nodes, :vsphere],
      vsphere: [ :aws, :existing_nodes ],
      existing_nodes: [ :aws, :vsphere ]
    }.freeze

    attr_reader :config, :render_arch
    def initialize(config)
      @config = config
    end

    def render(arch = nil)
      @render_arch = arch.to_sym unless arch.nil?
      @render_arch ||= config.architecture.to_sym

      unless AutomateCluster::Config::VALID_ARCHS.include?(@render_arch.to_s)
        raise AutomateCluster::Config::ConfigurationError, "#{@render_arch} is not a valid architecture, must be one of: #{AutomateCluster::Config::VALID_ARCHS.join(' ')}"
      end

      render_section config.save(true)
    end

    def special_key_chars(keys)
      keys.each do |value|
        return true if value.match?('-')
      end
      return false
    end

    def render_section(section_config, section = nil)
      return if render_arch && SKIP_SECTIONS.fetch(render_arch, []).include?(section)

      output = section_config.map do |k,v|
        if v.is_a?(Hash)
          # If the keys for the value hash have special characters we need to render it
          # as a hash instead of the block format
          if special_key_chars(v.keys)
            render_line(k, v, section)
          else
            render_section(v, k)
          end
        else
          render_line(k, v, section)
        end
      end.flatten.compact

      #skip empty sections
      return if output.empty?

      if section
        section_break = '#' * 63
        section_output = ''

        if SECTION_COMMENT.include?(section.to_sym)
          section_output += <<~SECTION_COMMENT
          #{section_break}
          ### #{SECTION_COMMENT[section.to_sym].ljust(55)} ###
          #{section_break}
          SECTION_COMMENT
        end

        section_output += <<~SECTION
        #{section} do
          #{output.join("\n  ")}
        end
        SECTION
      else
        output.join("\n")
      end
    end

    def valid_value?(value)
      return false if value.is_a?(String) && value.empty?
      return false if value.nil?

      true
    end

    def render_config_value(name, value)
      if value.is_a?(Hash)
        hash_value = ""
        PP.pp(value, hash_value)
        "#{name}(#{hash_value.chomp.gsub("\n", "\n\t")})"
      else
        "#{name} #{value.inspect}"
      end
    end

    def render_line(name, value, section)
      return if name == :blacklist
      return if config.blacklist.include?(name.to_s)
      return if config.blacklist.include?("#{section}.#{name}")

      # comment out invalid config values
      if valid_value?(value)
        line = render_config_value(name, value)
      else
        line = %Q{# #{name} ""}
      end

      comment = SETTING_COMMENT.fetch(section, {}).fetch(name.to_sym, nil)
      comments = Array(comment).map { |c| "### #{c}" }
      [comments, line].flatten.compact
    end
  end
end
