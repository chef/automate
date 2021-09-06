require 'erb'
require 'openssl'

module Automate
  module Backend
    class AutomateConfig
      include ERB::Util

      attr_accessor :svc_configs, :services, :overrides, :template_path, :render_path, :tls_key, :tls_cert

      def initialize(configs = {}, toml = {}, template_p, rendered_p)
        @svc_configs = configs
        @overrides = toml[:overrides]
        @services = toml[:services]
        @template_path = template_p
        @render_path = rendered_p
        if File.file?('/hab/user/automate-load-balancer/config/user.toml')
          # certs already set so we won't override
          @tls_key = nil
          @tls_cert = nil
        else
          @tls_key, @tls_cert = create_fqdn_cert
        end
      end

      def content(path)
        File.read(path)
      end

      def cn(pem)
        cert = OpenSSL::X509::Certificate.new(pem)
        subj_line = cert.subject.to_s
        cn_regex = /CN=([^, ]+)/
        matches = subj_line.match(cn_regex)
        cn_str = if !matches.nil?
                   matches[1]
                 else
                   'UNKNOWN'
                 end
        cn_str
      rescue OpenSSL::X509::CertificateError
        'CERTIFICATE_ERROR'
      end

      def create_fqdn_cert()
        fqdn_key = OpenSSL::PKey::EC.new('secp384r1')
        fqdn_pubkey = OpenSSL::PKey::EC.new('secp384r1')
        fqdn_key.generate_key
        fqdn_pubkey.public_key = fqdn_key.public_key
        fqdn_cert = OpenSSL::X509::Certificate.new
        fqdn_cert.version = 2
        fqdn_cert.serial = 2
        fqdn_cert.subject = OpenSSL::X509::Name.parse "CN=#{@overrides[:automate_fqdn]}"
        fqdn_cert.public_key = fqdn_pubkey
        fqdn_cert.not_before = Time.now
        # TODO: pass in override for expiration. Hard coded to 1 year for now
        fqdn_cert.not_after = fqdn_cert.not_before + 1 * 365 * 24 * 60 * 60 # 1 years validity
        ef = OpenSSL::X509::ExtensionFactory.new
        ef.subject_certificate = fqdn_cert
        ef.issuer_certificate = fqdn_cert
        fqdn_cert.add_extension(ef.create_extension('keyUsage','digitalSignature', true))
        fqdn_cert.add_extension(ef.create_extension('subjectKeyIdentifier','hash',false))
        fqdn_cert.sign(fqdn_key, OpenSSL::Digest::SHA256.new)
        [fqdn_key.to_pem, fqdn_cert.to_pem]
      end

      def render
        ERB.new(content(template_path), nil, '-').result(binding)
      end

      def save
        Log.info "Rendering #{render_path}"
        File.open(render_path, "w+") do |f|
          f.write(render)
        end
      end
    end
  end
end
