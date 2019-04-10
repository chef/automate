# encoding: utf-8
# author: Christoph Hartmann

require 'terminal-table'
require 'yaml'

# This script loads samples generated before. Those samples are used
# to estimate the amount of data that is generated over time.
class Stats
  def self.as_size(s)
    units = %w(B KiB MiB GiB TiB)

    size, unit = units.reduce(s.to_f) do |(fsize, _), utype|
      fsize > 512 ? [fsize / 1024, utype] : (break [fsize, utype])
    end

    "#{size > 9 || size.modulo(1) < 0.1 ? '%d' : '%.1f'} %s" % [size, unit]
  end

  def self.compare(minsize, fullsize)
    percentage = fullsize / minsize * 100
    "#{percentage.to_i}%"
  end

  # returns an array of the sample data
  def self.sample_data(platforms)
    rows = []
    min_sum = 0
    full_sum = 0
    platforms.each { |platform|
      platform_name = platform['name']

      min = File.size("report/samples/#{platform_name}-min.json").to_f
      min_sum += min
      full = File.size("report/samples/#{platform_name}-full.json").to_f
      full_sum += full
      rows << [platform_name, Stats.as_size(min), Stats.as_size(full), Stats.compare(min, full), platform['profiles'].length]
    }

    # calculate average
    min_average = min_sum / rows.length
    full_average = full_sum / rows.length

    [rows, min_average, full_average]
  end

  def self.estimate(sets, min_average, full_average)
    min_estimate_rows = []
    full_estimate_rows = []
    sets.each { |data|
      min_day = data['nodes'] * data['scan_per_day'] * min_average
      min = {
        data_per_day: min_day,
        data_per_month: min_day * 365/12,
        data_per_year: min_day * 365,
      }

      full_day = data['nodes'] * data['scan_per_day'] * full_average
      full = {
        data_per_day: full_day,
        data_per_month: full_day * 365/12,
        data_per_year: full_day * 365,
      }

      min_estimate_rows << [data['nodes'], data['scan_per_day'], 'min', Stats.as_size(min['data_per_day']), Stats.as_size(min[:data_per_month]), Stats.as_size(min[:data_per_year])]
      full_estimate_rows << [data['nodes'], data['scan_per_day'], 'full', Stats.as_size(full['data_per_day']), Stats.as_size(full[:data_per_month]), Stats.as_size(full[:data_per_year])]
    }

    min_estimate_rows + full_estimate_rows
  end
end
