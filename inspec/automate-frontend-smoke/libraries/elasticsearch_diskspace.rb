class ElasticsearchDiskspace < Inspec.resource(1)
  name 'elasticsearch_diskspace'

  desc 'Get disk usage information from elasticsearch'

  example <<~EXAMPLE
    describe elasticsearch_diskspace('http://localhost:9200') do
      its("available_percent.min") { should cmp > 10 }
    end
  EXAMPLE

  def initialize(url = nil)
    @url = url || 'http://localhost:9200'
  end

  def available_percent
    nodes.map do |node|
      percent('available_in_bytes', node)
    end
  end

  def available
    nodes.map do |node|
      gigs('available_in_bytes', node)
    end
  end

  def total
    nodes.map do |node|
      gigs('total_in_bytes', node)
    end
  end

  def nodes
    @nodes ||= read_es_data.values
  end

  def to_s
    max_length = nodes.map{ |d| d['host'].length }.max

    format = "%#{max_length+1}s - Total:%-3.1fGb Available:%3.1fGb (%-0.2f%%)"

    output = ["Elasticsearch disk space", '-'*40]
    nodes.each do |node|
      output << sprintf(
                        format, node['host'],
                        gigs('total_in_bytes', node),
                        gigs('available_in_bytes', node),
                        percent('available_in_bytes', node)
                )
    end

    output.join("\n")
  end

  private

  def gigs(name, data)
    data['fs']['total'][name].to_f / (1073741824)
  end

  def percent(name, data)
    data['fs']['total'][name].to_f / data['fs']['total']['total_in_bytes'] * 100
  end

  def read_es_data
    fs_stats_url = File.join(@url, '_nodes/stats/fs')
    resp = inspec.http(fs_stats_url)

    if resp.status == 200
      JSON.parse(resp.body)['nodes']
    else
      raise "Unable to parse node data from Elasticsearch, request status #{resp.status} - #{resp.body}"
    end
  end
end
