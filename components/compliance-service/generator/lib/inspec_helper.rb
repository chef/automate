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
end
