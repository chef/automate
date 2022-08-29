package majorupgradechecklist

const patchOpensearchSettingsTemplate = `
[opensearch]
  [opensearch.v1]
    [opensearch.v1.sys]
	{{ if .TotalShardSettings }}
      [opensearch.v1.sys.cluster]
        max_shards_per_node = {{ .TotalShardSettings }} # Refer the value from ElasticSearch Config, If this value is not there in elastic search config, then do not patch in openseaarch.
	{{ end }}
	{{ if .IndicesBreakerTotalLimit }}
      [opensearch.v1.sys.indices]
        [opensearch.v1.sys.indices.breaker]
          total_limit = "{{ .IndicesBreakerTotalLimit }}%"
	{{ end }}
      [opensearch.v1.sys.runtime]
	  {{ if .RuntimeMaxOpenFile }}
        max_open_files = "{{ .RuntimeMaxOpenFile }}"
	  {{ end }}
	  {{ if .RuntimeMaxLockedMem }}
        max_locked_memory = "{{ .RuntimeMaxLockedMem }}"
	  {{ end }}
	  {{ if .HeapMemory }}
        heapsize = "{{ .HeapMemory }}g"
	  {{ end }}
`
