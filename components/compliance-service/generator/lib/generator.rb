# encoding: utf-8
# author: Christoph Hartmann

# generator helper
require 'forgery'
require 'yaml'
require 'securerandom'
require 'colorize'
require 'json'

class Generator

  ENVIRONMENTS = ['DevSec Prod Alpha', 'DevSec Test Beta', 'DevSec Dev Delta', 'DevSec Dev_Gamma', 'DevSec Prod Zeta', 'Dot.Comma,Big;"Trouble']

  ROLES = [
    ["base_deb", "apache_deb", "debian-hardening-prod", "dot.role"],
    ["base_linux", "apache_linux", "linux-hardening-prod", "dot.role"],
    ["base_windows", "windows-hardening", "best.role.ever"]
  ]

  RECIPES = [
    ["apache_extras", "apache_extras::harden", "java::default", "nagios::fix"],
    ["java", "java::test", "java::security", "nagios::fix"],
    ["linux::harden","tomcat", "tomcat::setup", "tomcat::configure", "nagios::fix"],
    ["apache::default","tomcat", "tomcat::setup", "tomcat::configure", "nagios"]
  ]

  def self.generate_node_name
    [Forgery::Basic.color, Forgery::Address.street_name.split(' ').first].join('-').downcase
  end

  def self.generate_nodes(nodes, platforms, nodes_count)
    # add missing nodes until we have enough
    while nodes.length < nodes_count do
      node = {
        # TODO: we can have multiple nodes with the same node name
        'node_name' => generate_node_name,
        'environment' => ENVIRONMENTS.shuffle.first,
        'roles' =>  ROLES.shuffle.first,
        'recipes' => RECIPES.shuffle.first,
        'node_uuid' => SecureRandom.uuid,
        'platform_name' => platforms[rand(0..platforms.length-1)]['name']
      }
      nodes.push(node)
    end
    nodes
  end

  def self.load_sample_report(platform_name, format)
    file = File.open("report/samples/#{platform_name}-#{format}.json", 'rb')
    report = JSON.parse(file.read)
    report
  end

  def self.interval_to_s(minutes)
    hours = minutes / 60.0
    val = ''
    if hours < 24
      val = sprintf("%g", hours) + ' hour'
    else
      val = sprintf("%g", hours/24) + ' day'
    end
    val = val + 's' if val !~ /^1 /
    val
  end

  def self.interval_minutes(nodes_count, index, max_scans_a_day)
    div = index / nodes_count.to_f
    if    div <= 0.1 # 10%
      1440 / max_scans_a_day
    elsif div <= 0.4 # 30%
      1440
    elsif div <= 0.8 # 40%
      10080
    else             # 20%
      43200
    end
  end


  # iterate over node
  def self.generate_reports(nodes, platforms, simulation, handler)
    end_time = Time.now
    nodes_count = simulation['nodes'] < nodes.length ? simulation['nodes'] : nodes.length
    puts "---> Generating reports over a period of " + simulation['days'].to_s.yellow + " day(s)"
    total_scans = 0
    node_index = 0
    total_max_scans = simulation['total_max_scans']
    while node_index < nodes_count && total_scans < total_max_scans do
      node = nodes[node_index]
      sample_report = load_sample_report(node['platform_name'], simulation['sample_format'])
      interval = interval_minutes(nodes_count, node_index+1, simulation['max_scans'])
      puts time = Time.now.iso8601 + "---> Generating reports for node " + node['node_name'].yellow + "(#{node_index+1}/#{nodes_count}) with an interval of " + interval_to_s(interval).yellow + ", scans so far: #{total_scans}"
      max_scans_node = (simulation['days'] * 24 * 60) / interval + 1
      scan_index = max_scans_node
      while scan_index > 0 && total_scans < total_max_scans
        scan_index -= 1
        report = sample_report.dup
        report['node_name']   = node['node_name']
        report['node_uuid']   = node['node_uuid']
        report['environment'] = node['environment']
        report['roles']       = node['roles']
        report['recipes']     = node['recipes']
        report['report_uuid'] = SecureRandom.uuid
        report['end_time']    = (end_time - interval * scan_index * 60).utc.strftime('%FT%TZ')
        # puts report['end_time']
        # puts "* #{scan_index} #{interval} #{report['end_time']} #{node['node_name']}"
        handler.call(node['node_name'], report['end_time'], report)
        print scan_index if scan_index > 0 && scan_index % 500 == 0
      end
      total_scans += max_scans_node
      node_index += 1
      puts ''
    end
    puts "---> Generated " + total_scans.to_s.red + " reports in " + (Time.now - end_time).to_i.to_s.red + " seconds!"
  end
end
