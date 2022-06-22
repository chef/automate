require 'openssl'
require 'tempfile'
require_relative 'certificate_validator'

module AutomateCluster
  module Workstation
    class NotFound < StandardError; end

    class CertificateStore
      attr_accessor :errors

      attr_reader :certificates, :keys, :root_ca, :validator, :utils
      attr_reader :certificate_filename, :key_filename, :root_ca_filename

      def initialize(utils)
        clear_stores
        @utils = utils
        @validator = CertificateValidator.new(self)
      end

      def clear_stores 
        @errors = []
        @certificates = []
        @keys = []
        @root_ca = nil
        @full_cn_reversed = nil
        @short_cn = nil
      end

      # Make sure we output the certificate in the correct format
      def certificate_output
        File.read(certificate_filename) if File.exist?(certificate_filename)
      end

      # Make sure we output the key in the correct PKCS#8 format
      # https://opendistro.github.io/for-elasticsearch-docs/docs/security-configuration/tls/
      def key_output
        contents = ''

        cmd = <<~EOC
          hab pkg exec core/openssl openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "#{key_filename}" -topk8 -nocrypt
        EOC
        utils.backend_logger.debug "Converting #{key_filename} to PKCS8 format"

        utils.run_command(cmd).stdout
      end

      # ES requires the subject CN in reverse of the standard order
      def full_cn_reversed
        # Final gsub is a workaround for odfe not handling postalCode fields correctly
        @full_cn_reversed ||= certificates.first.subject.to_s.split(%r{/}).reverse.join(',').gsub(/,$/, '').gsub(/postalCode/i,'OID.2.5.4.17')
      end

      def short_cn
        @short_cn ||= full_cn_reversed.match(%r{^CN=(.*?),}i)[1]
      end

      # Load the given certificate and key files
      def load(cert_filename, key_filename = nil, root_filename = nil)
        clear_stores 
        @certificate_filename = cert_filename 
        @key_filename = key_filename
        @root_ca_filename = root_filename

        unless root_filename.nil?
          safe_read_root(root_filename)
        end
        unless key_filename.nil?
          safe_read_keys(key_filename)
        end

        safe_read_certs(cert_filename)
      end

      # Reads root certificate and store the value
      def safe_read_root(filename)
        @root_ca = read_certs!(filename).first

        return !root_ca
      rescue NotFound, OpenSSL::X509::CertificateError => e
        @errors << e.full_message

        return false
      end

      # Reads certificates and store the value
      def safe_read_certs(filename)
        @certificates = read_certs!(filename)

        return !certificates.empty?
      rescue NotFound, OpenSSL::X509::CertificateError => e
        @errors << e.full_message

        return false
      end

      # Reads certificate file
      def read_certs!(filename)
        raise NotFound, "Could not find certficate file: #{filename}" unless File.exist?(filename)

        raw_contents = File.read(filename)

        # check for multiple certs embedded together
        certs = raw_contents.scan(/(-----BEGIN CERTIFICATE-----.*?-----END CERTIFICATE-----)/m).flatten
        raise NotFound, "Could not find any certificates in #{filename}" if certs.empty? 

        certs.collect do |content|
          OpenSSL::X509::Certificate.new(content) 
        end
      end

      # Reads the key file and stores the value
      def safe_read_keys(filename)
        @keys = read_keys!(filename)
        return !keys.empty?
      rescue NotFound, OpenSSL::PKey::RSAError => e
        @errors << e.full_message
        return false
      end

      # Reads the key file
      def read_keys!(filename)
        raise NotFound, "Could not find key file: #{filename}" unless File.exist?(filename)

        raw_contents = File.read(filename)
        keylist = raw_contents.scan(/(-----BEGIN .*?RIVATE KEY-----.*?-----END .*?PRIVATE KEY-----)/m).flatten
        raise NotFound, "Could not find any private keys in #{filename}" if keylist.empty? 

        keylist.collect do |key|
          OpenSSL::PKey::RSA.new(key)
        end
      end
    end    
  end
end
