require 'json'

class InspecHelper
  # A control can have multiple tests. Returns 'passed' unless any of the results has a status different than 'passed'
  # A control without tests(results) is considered 'passed' at the moment
  def self.control_status(results)
    status = 'passed'
    return status unless results.is_a?(Array)
    results.each do |result|
      return 'failed' if result['status'] == 'failed'
      status = 'skipped' if result['status'] == 'skipped'
    end
    status
  end

  # Returns a compliance status string based on the passed/failed/skipped controls
  def self.compliance_status(counts)
    return 'unknown' unless counts.is_a?(Hash) &&
                            counts['failed'].is_a?(Hash) &&
                            counts['skipped'].is_a?(Hash)
    if counts['failed']['total'] > 0
      'failed'
    elsif counts['total'] == counts['skipped']['total']
      'skipped'
    else
      'passed'
    end
  end

  # Returns a string with the control criticality based on the impact value
  def self.impact_to_s(impact)
    if impact < 0.4
      'minor'
    elsif impact < 0.7
      'major'
    else
      'critical'
    end
  end

  # Returns a hash with the counted controls based on their status and criticality
  # This is working on full-json reports
  # total: count for all controls in the report, e.g. 100
  # passed.total: count for all controls that executed successfully, e.g. 40
  # skipped.total: count for all skipped controls, e.g. 10
  # failed.total: count for all failed controls, e.g. 50
  # failed.minor/major/critical: split the total failed controls in 3 buckets based on the criticality,
  #  e.g. minor: 10, major: 15, critical: 25
  def self.count_controls(profiles)
    count = {
      'total' => 0,
      'passed' => {
        'total' => 0,
      },
      'skipped' => {
        'total' => 0,
      },
      'failed' => {
        'total' => 0,
        'minor' => 0,
        'major' => 0,
        'critical' => 0,
      },
    }
    return count unless profiles.is_a?(Array)

    profiles.each do |profile|
      next unless profile && profile['controls'].is_a?(Array)
      profile['controls'].each do |control|
        count['total'] += 1
        # ensure all impacts are float
        control['impact'] = control['impact'].to_f
        case control['status']
        when 'passed'
          count['passed']['total'] += 1
        when 'skipped'
          count['skipped']['total'] += 1
        when 'failed'
          count['failed']['total'] += 1
          criticality = impact_to_s(control['impact'])
          count['failed'][criticality] += 1 unless criticality.nil?
        end
      end
    end
    count
  end

  # Returns the static profile information(i.e. no results) from a report profile
  def self.profile_from_report(report)
    # Marshal needed for a deep clone of the profile without changing the report as well
    profile = Marshal.load(Marshal.dump(report))
    profile['controls'].each { |control|
      # Need to remove the report results and leave only the static profile content
      control.delete('results')
      control['tags'] = control['tags'].to_json if control['tags']
      control['refs'] = control['refs'].to_json if control['refs']
    }

    profile['attributes'].each { |attribute|
      if attribute['options'].is_a?(Hash)
        attribute['options']['default'] = attribute['options']['default'].to_json
      end
    } unless profile['attributes'].nil?

    profile['doc_version'] = '1'
    profile
  end

  # Create a profiles report without the static profile information that's available in inspec_profile documents
  # We use this for the inspec_report ElasticSearch documents
  def self.profiles_min_from_report(report_profiles)
    profiles = Marshal.load(Marshal.dump(report_profiles))
    profiles.map { |profile|
      profile_min = {}
      profile_min['name']    = profile['name']
      profile_min['version'] = profile['version']
      profile_min['sha256']  = profile['sha256']
      profile_min['controls'] = []
      profile['controls'].each { |control|
        control_min = {}
        if control['results']
          control['results'].each { |result|
            result.delete('start_time')
            result.delete('resource')
            result
          }
          # Only a few control fields stay in the report
          %w(id impact results).each { |key|
            control_min[key] = control[key]
          }
        end
        control_min['status'] = InspecHelper.control_status(control['results'])
        profile_min['controls'].push(control_min)
      }
      profile_min
    }
  end

  # Returns a profiles_sums hash to use for the inspec_summary ElasticSearch documents
  def self.profiles_sums_from_profiles_min(profiles_min)
    profiles_sums = Marshal.load(Marshal.dump(profiles_min))
    profiles_sums.map { |profile_min|
      profile_sum = {}
      profile_sum['profile'] = profile_min['name']+'|'+profile_min['sha256']
      profile_sum['controls'] = InspecHelper.count_controls([profile_min])
      profile_sum
    }
  end


  def self.attach_result_to_control(controls, res)
    return unless controls.is_a?(Array) && res.is_a?(Hash)
    return if res['results'].nil?
    ctrl = controls.find { |i| i['id'] == res['id'] }
    if ctrl.nil?
      controls.push(res)
    else
      ctrl['results'] = res['results']
      ctrl['code'] = res['code'] if ctrl['code'].to_s.empty?
    end
  end

  def self.walk_all_profiles(prof, all_profiles, &block)
    return [] unless prof.is_a?(Hash) && prof['depends'].is_a?(Array)
    prof['depends'].map do |i|
      dep_prof = all_profiles.find { |j| j['name'] == i['name'] }
      [block.call(dep_prof)] + walk_all_profiles(dep_prof, all_profiles, &block)
    end.flatten.uniq
  end

  def self.all_profile_dependencies(profile, all_profiles)
    walk_all_profiles(profile, all_profiles) do |prof|
      prof['name']
    end
  end

  def self.all_profile_results(profile, all_profiles)
    walk_all_profiles(profile, all_profiles) do |prof|
      next [] unless prof['controls'].is_a?(Array)
      prof['controls'].find_all { |i| !i['results'].nil? }
    end
  end

  # Updates profiles so they include all controls from the profiles they depend on
  # Dependent profiles will be removed from the list of profiles
  def self.fix_inherited_profiles(report)
    dependencies = []
    prof = report['profiles'].find_all do |x|
      # skip dependent profiles
      next false if x.nil? || dependencies.include?(x['name'])
      # next profile if there are no dependencies
      next true if x['depends'].nil?
      # make sure we don't include dependencies anymore
      dep_names = all_profile_dependencies(x, report['profiles'])
      dependencies += dep_names
      # find dependencies and their results
      res_ctrls = all_profile_results(x, report['profiles'])
      # merge back results
      res_ctrls.compact.flatten.each do |ctl|
        attach_result_to_control(x['controls'], ctl)
      end
      true
    end
    report['profiles'] = prof
    report
  end


end
