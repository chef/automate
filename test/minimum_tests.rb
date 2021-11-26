require 'assert'
require_relative 'lib/backend_utils.rb'

class MinimumTests < Assert::Context
  desc '✯'

  harness = BackendUtils::Common.new

  test 'Habitat elects a PostgreSQL topology leader' do
    leader = harness.pg_hab_elected_leader
    assert_true !leader.nil?
  end

  test 'PostgreSQL leader is a functioning read/write database' do
    leader = harness.pg_promoted_leader
    assert_true !leader.nil?
    codes = harness.run_pgbench(harness.postgresql_private_ips)
    assert_equal [], codes.reject(&:zero?)
  end

  test 'Terraform outputs AWS PostgreSQL cluster public IPs' do
    pub_ips = harness.postgresql_private_ips
    assert_true !pub_ips.empty?
  end

  test 'Terraform outputs AWS PostgreSQL cluster private IPs' do
    priv_ips = harness.postgresql_private_ips
    assert_true !priv_ips.empty?
  end
end
