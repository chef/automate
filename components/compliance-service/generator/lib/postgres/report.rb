# encoding: utf-8

class PGReporter
  def self.report(dest)
    puts '---> Saving reports to ' + 'PostgreSQL'.red
    # Create postgres connection
    db = PG.connect({
      host: dest['target_opts']['host'],
      port: dest['target_opts']['port'],
      user: dest['target_opts']['user'],
      password: dest['target_opts']['password'],
      dbname: dest['target_opts']['dbname'],
    })
    # Create tables
    table_prefix = dest['target_opts']['table_prefix'] || ''
    # Profiles table
    db.exec %(DROP TABLE IF EXISTS #{table_prefix}profiles CASCADE)
    db.exec %(
      CREATE TABLE #{table_prefix}profiles(
        id    text PRIMARY KEY,
        title text
      )
    )
    # Controls table
    db.exec %(DROP TABLE IF EXISTS #{table_prefix}controls CASCADE)
    db.exec %(
      CREATE TABLE #{table_prefix}controls(
        id          text,
        profile_id  text REFERENCES #{table_prefix}profiles (id),
        title       text,
        description text,
        impact      decimal DEFAULT 0
      )
    )
    # Nodes table
    db.exec %(DROP TABLE IF EXISTS #{table_prefix}nodes CASCADE)
    db.exec %(
      CREATE TABLE #{table_prefix}nodes(
        id       uuid PRIMARY KEY,
        name     text,
        platform text
      )
    )
    # Scans table
    db.exec %(DROP TABLE IF EXISTS #{table_prefix}scans CASCADE)
    db.exec %(
      CREATE TABLE #{table_prefix}scans(
        id       uuid PRIMARY KEY,
        node_id  uuid REFERENCES #{table_prefix}nodes (id),
        end_time timestamp,
        duration decimal DEFAULT 0
      )
    )
    # Scan results table
    db.exec %(DROP TABLE IF EXISTS #{table_prefix}scan_results)
    db.exec %(
      CREATE TABLE #{table_prefix}scan_results(
        id         uuid,
        scan_id    uuid REFERENCES #{table_prefix}scans (id),
        node_id    uuid REFERENCES #{table_prefix}nodes (id),
        profile_id text REFERENCES #{table_prefix}profiles (id),
        control_id text,
        status     text,
        code_desc  text,
        message    text,
        start_time timestamp,
        duration   decimal DEFAULT 0
      )
    )
    # Populate nodes table
    nodes = YAML.load_file('report/nodes.yml')
    nodes.each do |node|
      db.exec %(
        INSERT INTO #{table_prefix}nodes (id, name, platform)
        VALUES ('#{node['node_uuid']}', '#{node['node_name']}', '#{node['platform_name']}')
      )
    end
    # Populate profiles table
    Dir.glob('report/profile/*.json') do |profile_path|
      profile = JSON.parse(File.read(profile_path))
      db.exec %(
        INSERT INTO #{table_prefix}profiles (id, title)
        VALUES ('#{profile['name']}', '#{profile['title']}')
      )
      # Populate controls table
      profile['controls'].each do |control|
        title = control['title'] ? db.escape_string(control['title']) : nil
        desc = control['desc'] ? db.escape_string(control['desc']) : nil
        db.exec %(
          INSERT INTO #{table_prefix}controls (id, profile_id, title, impact, description)
          VALUES (
            '#{control['id']}',
            '#{profile['name']}',
            '#{title}',
            '#{control['impact']}',
            '#{desc}'
          )
        )
      end
    end
    # Create report handler
    handler = lambda do |node_name, timestamp, report|
      # Insert scan
      db.exec %(
        INSERT INTO #{table_prefix}scans (id, node_id, end_time, duration)
        VALUES (
          '#{report['report_id']}',
          '#{report['node_uuid']}',
          '#{report['end_time']}',
          '#{report['statistics']['duration']}'
        )
      )
      case sample_format = matrix['simulation']['sample_format']
      when 'min'
        report['controls'].each do |control|
          desc = control['code_desc'] ? db.escape_string(control['code_desc']) : nil
          mess = control['message'] ? db.escape_string(control['message']) : nil
          db.exec %(
            INSERT INTO #{table_prefix}scan_results (
              scan_id, node_id, profile_id, control_id,
              status, code_desc, message
            )
            VALUES (
              '#{report['report_id']}',
              '#{report['node_uuid']}',
              '#{control['profile_id']}',
              '#{control['id']}',
              '#{control['status']}',
              '#{desc}',
              '#{mess}'
            )
          )
        end
      when 'full', 'full-elastic'
        report['profiles'].each do |profile|
          profile['controls'].each do |control|
            return if control['results'].nil?
            control['results'].each do |result|
              desc = result['code_desc'] ? db.escape_string(result['code_desc']) : nil
              mess = result['message'] ? db.escape_string(result['message']) : nil
              db.exec %(
                INSERT INTO #{table_prefix}scan_results (
                  scan_id, node_id, profile_id, control_id,
                  status, code_desc, message
                )
                VALUES (
                  '#{report['report_id']}',
                  '#{report['node_uuid']}',
                  '#{profile['name']}',
                  '#{control['id']}',
                  '#{result['status']}',
                  '#{desc}',
                  '#{mess}'
                )
              )
            end
          end
        end
      else
        fail "ERROR: Invalid sample format '#{sample_format}'"
      end
      print '.'
    end
    handler
  end
end
