require 'assert'
require_relative 'lib/backend_utils.rb'

class DeepTests < Assert::Context
  desc '✯'

  grace_period_s = 12
  harness = BackendUtils::Common.new

  test 'PostgreSQL survives reboot of current leader' do
    host_ips = harness.pg_public_ips.shuffle.shuffle
    harness.ping_until_alive(host_ips)
    leader = harness.pg_hab_elected_leader
    original_leader = leader
    harness.reboot(leader)
    harness.ping_until_alive(host_ips)
    leader = harness.pg_hab_elected_leader
    assert_true !leader.nil?
    assert_not_equal leader, original_leader
    sleep grace_period_s
    codes = harness.run_pgbench(harness.pg_public_ips)
    assert_equal [], codes.reject(&:zero?)
  end

  test 'PostgreSQL survives a controlled rolling cluster reboot' do
    host_ips = harness.pg_public_ips.shuffle.shuffle
    harness.ping_until_alive(host_ips)
    harness.rolling_reboot(host_ips)
    leader = harness.pg_hab_elected_leader
    assert_true !leader.nil?
    sleep grace_period_s
    codes = harness.run_pgbench(harness.pg_public_ips)
    assert_equal [], codes.reject(&:zero?)
  end

  test 'PostgreSQL survives a fast rolling cluster reboot' do
    host_ips = harness.pg_public_ips.shuffle.shuffle
    harness.ping_until_alive(host_ips)
    harness.rolling_reboot(host_ips, fast: true)
    harness.ping_until_alive(host_ips)
    leader = harness.pg_hab_elected_leader
    assert_true !leader.nil?
    sleep grace_period_s
    codes = harness.run_pgbench(harness.pg_public_ips)
    assert_equal [], codes.reject(&:zero?)
  end

  test 'PostgreSQL survives fast rolling cluster automate-ha-postgresql restart' do
    host_ips = harness.pg_public_ips.shuffle.shuffle
    harness.ping_until_alive(host_ips)
    harness.rolling_svc_restart(host_ips, 'chef/automate-ha-postgresql', fast: true)
    harness.ping_until_alive(host_ips)
    leader = harness.pg_hab_elected_leader
    assert_true !leader.nil?
    sleep grace_period_s
    codes = harness.run_pgbench(harness.pg_public_ips)
    assert_equal [], codes.reject(&:zero?)
  end
end
