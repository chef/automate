resource "null_resource" "generateMetadataToken" {
  provisioner "local-exec" {
    command = "echo `curl -X PUT http://169.254.169.254/latest/api/token -H 'X-aws-ec2-metadata-token-ttl-seconds: 21600'` > ${path.module}/metadata_token.txt"
  }
}

data "local_file" "readToken" {
  filename   = "${path.module}/metadata_token.txt"
  depends_on = [null_resource.generateMetadataToken]
}
data "http" "getEc2PrivateIP" {
  url = "http://169.254.169.254/latest/meta-data/local-ipv4"
  request_headers = {
    X-aws-ec2-metadata-token = chomp(data.local_file.readToken.content)
  }
}