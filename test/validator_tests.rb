require 'assert'
require_relative 'lib/certificate_store'
require_relative 'lib/backend_utils.rb'

class ValidatorTests < Assert::Context
  desc '✯'

  harness = BackendUtils::Common.new
  cert_path = File.expand_path('../certs/example', __dir__)
  certstore = AutomateCluster::Workstation::CertificateStore.new(harness)
  validator = certstore.validator 

  test "should load cert file" do
    certstore.clear_stores
    assert certstore.safe_read_certs(File.join(cert_path, 'es_admin_ssl_public.pem'))
    assert certstore.certificates.length > 0
  end

  test "shouldn't load invalid cert file" do
    certstore.clear_stores
    certstore.safe_read_certs(File.join(cert_path, 'es_admin_ssl_private.key'))
    assert certstore.certificates.empty?
    assert certstore.errors.length > 0
  end  

  test "shouldn't load fake cert file" do
    certstore.clear_stores
    assert_not certstore.safe_read_certs(File.join(cert_path, 'fake.pem'))
    assert certstore.certificates.empty?
  end

  test "should load key file" do
    certstore.clear_stores
    assert certstore.safe_read_keys(File.join(cert_path, 'es_ssl_private.key'))
    assert_not certstore.keys.empty?    
  end

  test "shouldn't load fake key file" do
    certstore.clear_stores
    assert_not certstore.safe_read_keys(File.join(cert_path, 'fake.key'))
    assert certstore.keys.empty?
    assert certstore.errors.length > 0
  end

  test "shouldn't load invalid key file" do
    certstore.clear_stores
    assert_not certstore.safe_read_keys(File.join(cert_path, 'ca_root.pem'))
    assert certstore.keys.empty?
    assert certstore.errors.length > 0
  end

  test "should load all the given files" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    root_ca = File.join(cert_path, 'ca_root.pem')

    assert certstore.load(cert, key, root_ca)
    assert_not certstore.certificates.empty?
    assert_not certstore.keys.empty?
    assert certstore.root_ca
  end

  test "should find tls_auth extensions" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    certstore.load(cert, key)

    assert validator.has_tls_auth?
  end

  test "should not find tls_auth extensions" do
    certstore.clear_stores
    certstore.safe_read_certs(File.join(cert_path, 'ca_root.pem'))
    assert_not validator.has_tls_auth?
  end

  test "should verify certificate with key" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    certstore.load(cert, key)

    assert validator.verify_key?
  end

  test "shouldn't verify certificate with wrong key" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_ssl_private.key')
    certstore.load(cert, key)

    assert_not validator.verify_key?
  end

  test "shouldn't verify certificate with no key" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'fake.key')
    certstore.load(cert, key)

    assert_not validator.verify_key?
  end  

  test "should verify certificate chain with root ca" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    root_ca = File.join(cert_path, 'ca_root.pem')
    certstore.load(cert, key, root_ca)

    assert validator.verify_cert_chain?
  end

  test "should verify certificate chain without root ca" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    certstore.load(cert, key)

    assert validator.verify_cert_chain?
    assert_empty validator.errors
  end

  test "shouldn't verify certificate chain with wrong root ca" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    root_ca = File.join(cert_path, 'es_admin_ssl_public.pem')
    certstore.load(cert, key, root_ca)

    assert_not validator.verify_cert_chain?
  end

  test "shouldn't verify certificate chain with wrong root ca" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    root_ca = File.join(cert_path, 'es_admin_ssl_public.pem')
    certstore.load(cert, key, root_ca)

    assert_not validator.verify_cert_chain?
  end

  test "should generate the same key" do
    cert = File.join(cert_path, 'es_admin_ssl_public.pem')
    key = File.join(cert_path, 'es_admin_ssl_private.key')
    certstore.load(cert, key)

    assert_equal certstore.key_output, File.read(key)
  end    
end
