#!/usr/bin/env ruby
# encoding: utf-8
# author: Christoph Hartmann

require 'thor'
require 'fileutils'
require 'net/http'
require 'colorize'
require 'pg'

require_relative 'lib/samples'
require_relative 'lib/generator'
require_relative 'lib/stats'

require_relative 'lib/automate/report'
require_relative 'lib/postgres/report'

def ensure_directories
  # create output directories
  FileUtils.mkdir_p 'report/profile'
  FileUtils.mkdir_p 'report/samples'
  FileUtils.mkdir_p 'report/nodes'
end

class GeneratorCLI < Thor
  desc 'sample', 'Runs InSpec against real machines to take samples'
  def sample
    profile_path = './.tmp/profiles/dist/supported/'
    if !Dir.exist?(profile_path) || Dir["#{profile_path}/*"].empty?
      puts "Cannot find #{profile_path}. Please run `make download-sample-profiles`"
      exit 1
    end

    sample_profiles_path = profile_path + '%{profile_name}.tar.gz'

    puts '---> Load samples from matrix.yml'
    matrix = YAML.load_file(File.join(__dir__, 'matrix.yml'))
    platforms = matrix['samples']['platforms']
    ensure_directories

    puts '---> Extract information form profiles'
    profiles = platforms.map { |entry| entry['profiles'] }.flatten.uniq.compact
    Samples.snap_profile_info(profiles, sample_profiles_path)

    puts '---> Take samples for platforms with json-min'
    puts "NOTE: Please ensure all targets are up and running."
    opts = {
      sample_profiles_path: sample_profiles_path,
      target: matrix['samples']['target'],
      key_files: matrix['samples']['key_files'],
    }

    Samples.snap_json_min_reports(platforms, opts)

    puts '---> Take samples for platforms with json'
    Samples.snap_json_full_reports(platforms, opts)
  end

  desc 'generate', 'Generates InSpec sample reports'
  option :dump, type: :boolean, aliases: 'd'
  option :matrix, aliases: 'm'
  def generate
    puts '---> Check that samples are available'

    # check if samples are not available
    if !Dir.exist?('report/samples/') || Dir['report/samples/*'].empty?
      sample
    end

    matrix_file = options[:matrix].nil? ? 'matrix.yml' : options[:matrix]
    puts "---> Load simulation configuration from #{matrix_file}"
    matrix = YAML.load_file(File.join(__dir__, matrix_file))
    platforms = matrix['samples']['platforms']
    nodes_count = matrix['simulation']['nodes']
    ensure_directories

    puts '---> Generate nodes'
    # load nodes file
    if File.exist?('report/nodes.yml')
      nodes = YAML.load_file(File.join(__dir__, 'report/nodes.yml'))
    else
      nodes = []
    end

    # fill up missing nodes
    new_nodes = Generator.generate_nodes(nodes, platforms, nodes_count)
    File.open('report/nodes.yml', 'w') {|f| f.write new_nodes.to_yaml }

    dest = YAML.load_file(File.join(__dir__, 'reports_dest.yml'))
    handler = nil
    case dest['target']
    when "filesystem"
      puts '---> Saving reports to ' + 'reports/nodes/'.red
      handler = lambda do |node_name, timestamp, report|
        File.open("report/nodes/#{node_name}-#{timestamp}.json", 'w') do |f|
          f.write(JSON.pretty_generate(report))
        end
        print '.'
      end
    when "automate"
      handler = AutomateReporter.report(dest, options[:dump])
    when "postgresql"
      handler = PGReporter.report(dest)
    else
      fail "ERROR: Invalid reports target '#{dest['target']}'"
    end
    Generator.generate_reports(new_nodes, platforms, matrix['simulation'], handler)
    puts '---> Generation of data finished'
  end

  desc 'stats', 'Outputs projections about data size'
  def stats

    # check if samples are not available
    if !Dir.exist?('report/samples/') || Dir['report/samples/*'].empty?
      sample
    end

    matrix = YAML.load_file(File.join(__dir__, 'matrix.yml'))

    puts "Sample Data:\n\n"
    platforms = matrix['samples']['platforms']
    rows, min_average, full_average = Stats.sample_data(platforms)
    table = Terminal::Table.new :headings => ['Name', 'Min-JSON', 'Full-JSON', 'Compare', 'Profiles'], :rows => rows do |t|
      t.add_separator
      t.add_row ["Average", Stats.as_size(min_average), Stats.as_size(full_average), Stats.compare(min_average, full_average), '']
    end
    puts table
    puts "\n\n"
    puts "Scaled Data (min-json: #{Stats.as_size(min_average)}, full-json: #{Stats.as_size(full_average)}):\n\n"
    sets = matrix['statistics']['sets']
    estimate_table = Terminal::Table.new :headings => ['Nodes', 'Scans per Day', 'Type', 'Data per Day', 'Data p Month', 'Data per Year'], :rows => Stats.estimate(sets, min_average, full_average)
    puts estimate_table
  end
end

GeneratorCLI.start(ARGV)
