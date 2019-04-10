# encoding: utf-8
# author: Christoph Hartmann

# load inspec
require 'inspec'
require 'inspec/log'
require 'inspec/dependencies/cache'
require 'digest'
# load the same enrichment library used by logstash(Automate)
require_relative './inspec_helper.rb'
require 'active_support/core_ext/hash/indifferent_access'

# load supermarket plugin, this is part of the inspec gem
require 'bundles/inspec-supermarket/api'
require 'bundles/inspec-supermarket/target'

class Samples

  DEFAULT_OPTS = {
    'logger' => Inspec::Log.init(STDERR)
  }

  def self.run_report(targets, opts)
    o = opts.dup
    runner = ::Inspec::Runner.new(o)
    targets.each { |target| runner.add_target(target) }
    runner.run
    runner.report
  end

  def self.run_command(command, opts)
    o = opts.dup
    runner = Inspec::Runner.new(o)
    res = runner.eval_with_virtual_profile(command)
    runner.load
    res
  end

  def self.json_profile(target, opts)
    o = opts.dup
    o[:ignore_supports] = true
    o[:backend] = Inspec::Backend.create(target: 'mock://')

    o[:cache] = o[:cache] || Inspec::Cache.new
    fetcher = Inspec::Profile.resolve_target(target, o[:cache])
    # identify profile hash, we need this to combine profile with report
    sha = fetcher.cache_key

    # return profile info
    profile = Inspec::Profile.for_fetcher(fetcher, o)
    profile.info
  end

  def self.enriched_report(content, opts)
    content['node_name']   = 'to_be_generated'
    content['node_uuid']   = 'to_be_generated'
    content['report_uuid'] = 'to_be_generated'
    content['end_time']    = 'to_be_generated'
    content['environment'] = 'to_be_generated'
    content['type']        = 'inspec_report'
    content
  end

  # This will not be produced by inspec and needs to be done by another service, like logstash/audit cookbook, etc
  # We are doing it in sample to avoid having to compute this for every scan in generate.rb
  def self.elastic_enriched_report(content)
    # to access the hash via both string and symbol keys
    content = content.with_indifferent_access
    # count controls across all profiles in this scan
    content['doc_version'] = '1'
    content['summary'] = {}
    content['summary']['profiles_sums'] = []

    # count controls per profile
    content['profiles'].each { |profile|
      profile_summary = { 'profile' => profile['name']+'|'+profile['sha256'] }
      profile['controls'].each { |control|
        control['tags'] = control['tags'].to_json if control['tags']
        control['refs'] = control['refs'].to_json if control['refs']
        control['status'] = InspecHelper.control_status(control['results'])
      }
      profile_summary['controls'] = InspecHelper.count_controls([profile])
      content['summary']['profiles_sums'].push(profile_summary)
      profile['attributes'].each { |attribute|
        if attribute['options'] && attribute['options']['default']
          attribute['options']['default'] = attribute['options']['default'].to_json
        end
      }
    } if content['profiles']
    content['summary']['controls'] = InspecHelper.count_controls(content['profiles'])
    content['summary']['status'] = InspecHelper.compliance_status(content['summary']['controls'])
    content
  end

  def self.snap_profile_info(profiles, sample_profiles_path)
    profile_opts = DEFAULT_OPTS.dup
    profiles.each { |profile_name|
      filename = sample_profiles_path % { profile_name: profile_name }
      info = json_profile(filename, profile_opts)
      File.open("report/profile/#{info[:name]}-#{info[:version]}.json", 'w') do |f|
        f.write(JSON.pretty_generate(info))
      end
    }
  end

  def self.snap_report(scan_opts, profiles, output)
    # TODO skip if target is nil and output a warning
    # TODO skip if profiles array is empty
    report = run_report(profiles, scan_opts)
    # inspec bug where json-min controls are included in full json as well:
    report.delete(:controls) if scan_opts['format'] == 'json' && report[:controls] != nil
    e_report = enriched_report(report, scan_opts)
    File.open(output, 'w') do |f|
      f.write(JSON.pretty_generate(e_report))
    end
  end

  # This is inspec's json-min format, but we are using a min that has the full data structure
  # def self.make_json_min(full_report)
  #   min_report = {}
  #   min_report[:version] = full_report[:version]
  #   min_report[:statistics] = full_report[:statistics]
  #   controls = []
  #   full_report[:profiles].each { |profile|
  #     profile[:controls].each { |control|
  #       if control[:results]
  #         control[:results].each { |result|
  #           min_control = {
  #             id: control[:id],
  #             profile_id: profile[:name],
  #             status: result[:status],
  #             code_desc: result[:code_desc]
  #           }
  #           min_control[:message] = result[:message] if result[:message]
  #           min_control[:skip_message] = result[:skip_message] if result[:skip_message]
  #           controls.push(min_control)
  #         }
  #       end
  #     }
  #   }
  #   min_report[:controls] = controls
  #   min_report
  # end

  # A minimal json format that inspec's full format data structure
  def self.make_json_min(full_report)
    # deep object clone
    new_report = Marshal.load( Marshal.dump(full_report))
    new_report[:profiles].each { |profile|
      profile[:controls].map { |control| control.delete(:results) }
    }
    new_report[:profiles_min] = []
    full_report[:profiles].each { |profile|
      controls = []
      profile[:controls].each { |control|
        min_control = {
          id: control[:id]
        }
        if control[:results]
          control[:results].map { |result|
            result.delete(:start_time)
            result.delete(:resource)
            result
          }
          control[:results].each { |result|
            min_control[:results] = control[:results]
          }
        end
        min_control[:status] = InspecHelper.control_status(control['results'])
        controls.push(min_control)
      }
      new_report[:profiles_min].push(
        {
          name: profile[:name],
          version: profile[:version],
          sha256: profile[:sha256],
          controls: controls
        }
      )
    }
    new_report
  end

  def self.generate_profile_path(profiles, template_path)
    profiles_paths = profiles.map { |entry| template_path % { profile_name: entry } }
  end

  def self.snap_json_min_reports(platforms, opts)
    platforms.each { |platform|
      platform_name = platform['name']

      scan_opts = DEFAULT_OPTS.dup
      scan_opts['format'] = 'json-min'
      scan_opts['target'] = platform['target'] || opts[:target]
      scan_opts['key_files'] = platform['key_files'] || opts[:key_files]
      snap_report(
        scan_opts, generate_profile_path(platform['profiles'],
        opts[:sample_profiles_path]),
        "report/samples/#{platform_name}-min.json"
      )
    }
  end

  def self.snap_json_full_reports(platforms, opts)
    platforms.each { |platform|
      platform_name = platform['name']

      scan_opts = DEFAULT_OPTS.dup
      scan_opts['format'] = 'json'
      scan_opts['target'] = platform['target'] || opts[:target]
      scan_opts['key_files'] = platform['key_files'] || opts[:key_files]
      snap_report(
        scan_opts, generate_profile_path(platform['profiles'],
        opts[:sample_profiles_path]),
        "report/samples/#{platform_name}-full.json"
      )
    }
  end
end
