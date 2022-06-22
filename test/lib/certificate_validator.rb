require 'openssl'

module AutomateCluster
  module Workstation
    class ChainError < StandardError; end

    class CertificateValidator
      attr_reader :certstore 

      def initialize(certstore)
        @certstore = certstore
      end

      def root_ca 
        certstore.root_ca
      end

      def certificates 
        certstore.certificates
      end

      def keys 
        certstore.keys 
      end

      def errors 
        certstore.errors
      end

      # Verify that the first certificate in the file was signed with the key
      def verify_key?
        return false if keys.first.nil?

        certificates.first.check_private_key(keys.first)
      end

      # Verify that the certificate chain is correct.  
      # The certificates should have the following order in the cert file:
      # Issued Certificate -> Immediate Certificate -> Root CA
      def verify_cert_chain?
        previous = nil
        
        certificates.each do |cert|
          if cert == certificates.last && root_ca
            # if we were provided a root ca we should verify that as well
            raise ChainError, "Invalid root ca #{cert.issuer} != #{root_ca.subject}" unless cert.issuer == root_ca.subject
          elsif cert != certificates.first
            # if we are not the first certificate than make sure the chain to the previous
            # cert if valid
            raise ChainError, "Invalid cert chain #{previous.issuer} != #{cert.subject}" unless previous.issuer == cert.subject
          end
          previous = cert
        end

        return true
      rescue ChainError => e
        errors << e.full_message

        return false
      end

      # ES Admin/Server certs require both if extendedKeyUsage is defined
      # we only care about the first cert in the file if there are multiple
      def has_tls_auth?
        certificates.first.extensions.each do |extension|
          ext = extension.to_s
          next unless ext.match?(/extendedKeyUsage/)
          return true if tls_auth_extension?(ext)
        end
        
        return false
      end

      # Checks the given extension to see if it contains the required tls auth
      def tls_auth_extension?(ext)
        ext.match?(/TLS Web Client Authentication/) && ext.match?(/TLS Web Server Authentication/)
      end
    end
  end
end
