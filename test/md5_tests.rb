require 'assert'
require_relative 'lib/backend_utils.rb'

class Md5Tests < Assert::Context
  desc '✯'

  harness = BackendUtils::Common.new(true)

  test 'PostgreSQL table md5sum with leader reboot' do
    host_ips = harness.postgresql_private_ips
    harness.ping_until_alive(host_ips)
    leader = harness.pg_promoted_leader
    original_hashsum = harness.pg_md5(leader)
    harness.reboot(leader)
    harness.ping_until_alive(leader)
    harness.backend_logger.info 'Pausing for grace period while services normalize..'
    sleep 120
    new_leader = harness.pg_promoted_leader
    new_hashsum = harness.pg_md5(new_leader)
    assert_equal original_hashsum, new_hashsum
  end

  test 'PostgreSQL replica table md5sum' do
    host_ips = harness.postgresql_private_ips
    harness.ping_until_alive(host_ips)
    leader = harness.pg_promoted_leader
    original_hashsum = harness.pg_md5(leader)
    replicas = host_ips - [leader]
    replicas.each do |replica|
      new_hashsum = harness.pg_md5(replica)
      assert_equal original_hashsum, new_hashsum
    end
  end
end
