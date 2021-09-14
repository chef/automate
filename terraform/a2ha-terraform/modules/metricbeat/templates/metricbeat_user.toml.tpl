tags = [${metricbeat_tags}]

#================================ Output ======================================
[output]
[output.elasticsearch]
hosts = [${elasticsearch_output_hosts}]
[output.elasticsearch.ssl]
#This should be set to "full" in production environments with valid certificates
verification_mode = "none"
