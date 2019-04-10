REDHAT_REPOS = attribute('REDHAT_REPOS',{
  title: 'redhat_repos',
  default: ["rhel-7-server-rpms/7Server/x86_64", "rhel-7-server-rt-beta-rpms/x86_64", "rhel-7-server-rt-rpms/7Server/x86_64"],
  description: 'define a list of enabled RedHat repositories',
})

REDHAT_PUBKEY = attribute('REDHAT_PUBKEY',{
  title: 'redhat_pubkey',
  default: ["gpg-pubkey-fd431d51-4ae0493b", "gpg-pubkey-2fa658e0-45700c69"],
  description: 'define a list of public keys, which are used for signing Red Hat software packages',
})

NTP_SERVER = attribute('NTP_SERVER',{
  title: 'ntp_server',
  default: ["0.rhel.pool.ntp.org iburst", "1.rhel.pool.ntp.org iburst", "2.rhel.pool.ntp.org iburst", "3.rhel.pool.ntp.org iburst"],
  description: 'define a list of NTP servers',
})

IPTABLES_RULES = attribute('IPTABLES_RULES',{
  title: 'iptables_rules',
  default: ["-A OUTPUT -p tcp -m state --state NEW,ESTABLISHED -j ACCEPT", "-A OUTPUT -p udp -m state --state NEW,ESTABLISHED -j ACCEPT", "-A OUTPUT -p icmp -m state --state NEW,ESTABLISHED -j ACCEPT", "-A INPUT -p tcp -m state --state ESTABLISHED -j ACCEPT", "-A INPUT -p udp -m state --state ESTABLISHED -j ACCEPT", "-A INPUT -p icmp -m state --state ESTABLISHED -j ACCEPT"],
  description: 'define a list of NTP servers',
})

AUDITD_MAX_LOG_FILE = attribute('AUDITD_MAX_LOG_FILE',{
  title: 'auditd_max_log_file',
  default: "8",
  description: 'Configure the maximum size of the audit log file.',
})

RSYSLOG_HOST = attribute('RSYSLOG_HOST',{
  title: 'rsyslog_host',
  description: 'Define the rsyslog server, e.g. loghost.example.com',
})

DESIGNATED_LOG_HOST = attribute('DESIGNATED_LOG_HOST',{
  title: 'designated_log_host',
  default: false,
  description: 'Configure, if the system is a designated log host. per default disabled',
})

MACS = attribute('MACS',{
  title: 'macs',
  default: "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com",
  description: 'Define a list of allowed MACs',
})

BLACKLIST = attribute('BLACKLIST',{
  default: ["/usr/bin/rcp", "/usr/bin/rlogin", "/usr/bin/rsh", "/usr/libexec/openssh/ssh-keysign", "/usr/lib/openssh/ssh-keysign", "/sbin/netreport", "/usr/sbin/usernetctl", "/usr/sbin/userisdnctl", "/usr/sbin/pppd", "/usr/bin/lockfile", "/usr/bin/mail-lock", "/usr/bin/mail-unlock", "/usr/bin/mail-touchlock", "/usr/bin/dotlockfile", "/usr/bin/arping", "/usr/sbin/arping", "/usr/sbin/uuidd", "/usr/bin/mtr", "/usr/lib/evolution/camel-lock-helper-1.2", "/usr/lib/pt_chown", "/usr/lib/eject/dmcrypt-get-device", "/usr/lib/mc/cons.saver"],
  description: 'blacklist of suid/sgid program on system',
})



control "xccdf_org.cisecurity.benchmarks_rule_1.8_Ensure_updates_patches_and_additional_security_software_are_installed" do
  title "Ensure updates, patches, and additional security software are installed"
  desc  "
    Periodically patches are released for included software either due to security flaws or to     include additional functionality.
    Rationale: Newer patches may contain security enhancements that would not be available through the latest full update. As a result, it is recommended that the latest software patches be used to take advantage of the latest functionality. As with any software installation, organizations need to determine if a given update meets their requirements and verify the compatibility and supportability of any additional software against the update revision that is selected.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": 1.8
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe ('Patch system') do
    skip 'Use Linux Patch Profile from the Dev-Sec Hardening Framework https://github.com/dev-sec/linux-patch-baseline'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.1_Ensure_mounting_of_cramfs_filesystems_is_disabled" do
  title "Ensure mounting of cramfs filesystems is disabled"
  desc  "
    The cramfs filesystem type is a compressed read-only Linux filesystem embedded in small footprint systems. A cramfs image can be used without having to first decompress the image.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the server. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('cramfs') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.2.1_Ensure_package_manager_repositories_are_configured" do
  title "Ensure package manager repositories are configured"
  desc  "
    Systems need to have package manager repositories configured to ensure they receive the latest patches and updates.
    
    Rationale: If a system's package repositories are misconfigured important patches may not be identified or a rogue repository could introduce compromised software.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.2.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  REDHAT_REPOS.each do |repository|
    describe yum.repo(repository) do
      it { should exist }
      it { should be_enabled }
    end
  end
  cmd = command('yum repolist enabled').stdout.split("\n")
  get_other_repos = cmd.slice(2..cmd.length-2) || []
  other_repos = get_other_repos.map { |repositories| repositories.gsub(/\s.+/, '') }
  other_repos -= REDHAT_REPOS
  unless other_repos.empty?
    other_repos.each do |repository|
      describe yum.repo(repository) do
        it { should_not exist }
        it { should_not be_enabled }
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.3.1_Ensure_AIDE_is_installed" do
  title "Ensure AIDE is installed"
  desc  "
    AIDE takes a snapshot of filesystem state including modification times, permissions, and file hashes which can then be used to compare against the current state of the filesystem to detect modifications to the system.
    
    Rationale: By monitoring the filesystem state compromised files can be detected to prevent or limit the exposure of accidental or malicious misconfigurations or modified binaries.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.3.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package("aide") do
    it { should be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.4.1_Ensure_permissions_on_bootloader_config_are_configured" do
  title "Ensure permissions on bootloader config are configured"
  desc  "
    The grub configuration file contains information on boot settings and passwords for unlocking boot options. The grub configuration is usually located at /boot/grub2/grub.cfg and linked as /etc/grub2.conf .
    
    Rationale: Setting the permissions to read and write for root only prevents non-root users from seeing the boot parameters or changing them. Non-root users who read the boot parameters may be able to identify weaknesses in security upon boot and be able to exploit them.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.4.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/boot/grub2/grub.cfg") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.5.1_Ensure_core_dumps_are_restricted" do
  title "Ensure core dumps are restricted"
  desc  "
    A core dump is the memory of an executable program. It is generally used to determine why a program aborted. It can also be used to glean confidential information from a core file. The system provides the ability to set a soft limit for core dumps, but this can be overridden by the user.
    
    Rationale: Setting a hard limit on core dumps prevents users from overriding the soft variable. If core dumps are required, consider setting limits for user groups (see limits.conf(5) ). In addition, setting the fs.suid_dumpable variable to 0 will prevent setuid programs from dumping core.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.5.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe limits_conf do
    its('*') { should include ['hard','core','0'] }
  end
  limit_confs_path = '/etc/security/limits.d/'
  check_confs = command("ls #{limit_confs_path}").stdout.split("\n").map { |config| limits_conf("/etc/security/limits.d/#{config}").read_params.to_s.include?('core') ? config : nil }.compact
  
  unless check_confs.empty?
    check_confs.each do |config|
      describe limits_conf("#{limit_confs_path}" + config) do
        its('*') { should include ['hard', 'core', '0'] }
      end
    end
  end
  describe kernel_parameter('fs.suid_dumpable') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.2_Ensure_GDM_login_banner_is_configured" do
  title "Ensure GDM login banner is configured"
  desc  "
    GDM is the GNOME Display Manager which handles graphical login for GNOME based systems.
    Rationale: Warning messages inform users who are attempting to login to the system of their legal status regarding the system and must include the name of the organization that owns the system and any monitoring policies that are in place.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.7.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  only_if { package('gdm').installed? }
  
  options = {
    assignment_regex: /^\s*([^:]*?)\s*:\s*(.*?)\s*$/
  }
  describe parse_config_file('/etc/dconf/db/gdm.d/01-banner-message').params['org/gnome/login-screen'] do
    its('banner-message-enable') { should eq 'true' }
    its('banner-message-text') { should_not eq '' }
  end
  describe parse_config_file('/etc/dconf/profile/gdm', options) do
    its('user-db') { should eq 'user' }
    its('system-db') { should eq 'gdm' }
    its('file-db') { should eq '/usr/share/gdm/greeter-dconf-defaults' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.1.1_Ensure_message_of_the_day_is_configured_properly" do
  title "Ensure message of the day is configured properly"
  desc  "
    The contents of the /etc/motd file are displayed to users after login and function as a message of the day for authenticated users.
    
    Unix-based systems have typically displayed information about the OS release and patch level upon logging in to the system. This information can be useful to developers who are developing software for a particular OS platform. If mingetty(8) supports the following options, they display operating system information: \\m - machine architecture \\r - operating system release \\s - operating system name \\v - operating system version
    
    Rationale: Warning messages inform users who are attempting to login to the system of their legal status regarding the system and must include the name of the organization that owns the system and any monitoring policies that are in place. Displaying OS and patch level information in login banners also has the side effect of providing detailed system information to attackers attempting to target specific exploits of a system. Authorized users can easily get this information by running the \" uname -a \" command once they have logged in.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.7.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/motd") do
    its("content") { should_not match(/(\\v|\\r|\\m|\\s)/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.1_Ensure_chargen_services_are_not_enabled" do
  title "Ensure chargen services are not enabled"
  desc  "
    chargenis a network service that responds with 0 to 512 ASCII characters for each connection it receives. This service is intended for debugging and testing purposes. It is recommended that this service be disabled.
    
    Rationale: Disabling this service will reduce the remote attack surface of the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  if package('xinetd').installed?
    describe xinetd_conf.services('chargen') do
      it { should be_disabled }
    end
  else
    describe package('xinetd') do
      it { should_not be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.2_Ensure_X_Window_System_is_not_installed" do
  title "Ensure X Window System is not installed"
  desc  "
    The X Window System provides a Graphical User Interface (GUI) where users can have multiple windows in which to run programs and various add on. The X Windows system is typically used on workstations where users login, but not on servers where users typically do not login.
    
    Rationale: Unless your organization specifically requires graphical login access via X Windows, remove it to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.2"
  tag "level": "1"
  tag "type": "Server"
  describe packages(/^xorg-x11.*/) do
    its("entries") { should be_empty }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.1.1_Ensure_time_synchronization_is_in_use" do
  title "Ensure time synchronization is in use"
  desc  "
    System time should be synchronized between all systems in an environment. This is typically done by establishing an authoritative time server or set of servers and having all systems synchronize their clocks to them.
    
    Rationale: Time synchronization is important to support time sensitive security mechanisms like Kerberos and also ensures log files have consistent time records across the enterprise, which aids in forensic investigations.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "2.2.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe.one do
    describe package('ntp') do
      it { should be_installed }
    end
    describe package('chrony') do
      it { should be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.3.1_Ensure_NIS_Client_is_not_installed" do
  title "Ensure NIS Client is not installed"
  desc  "
    The Network Information Service (NIS), formerly known as Yellow Pages, is a client-server directory service protocol used to distribute system configuration files. The NIS client ( ypbind ) was used to bind a machine to an NIS server and receive the distributed configuration files.
    
    Rationale: The NIS service is inherently an insecure system that has been vulnerable to DOS attacks, buffer overflows and has poor authentication for querying NIS maps. NIS generally has been replaced by such protocols as Lightweight Directory Access Protocol (LDAP). It is recommended that the service be removed.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.3.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('ypbind') do
    it { should_not be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.7_Ensure_wireless_interfaces_are_disabled" do
  title "Ensure wireless interfaces are disabled"
  desc  "
    Wireless networking is used when wired networks are unavailable. Red Hat Enterprise Linux 7 contains a wireless tool kit to allow system administrators to configure and use wireless networks.
    
    Rationale: If wireless is not to be used, wireless devices can be disabled to reduce the potential attack surface.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": 3.7
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'disable wireless interfaces' do
    skip 'disable all wireless interfaces, to determine wireless interfaces run iwconfig command'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.1.1_Ensure_IP_forwarding_is_disabled" do
  title "Ensure IP forwarding is disabled"
  desc  "
    The net.ipv4.ip_forward flag is used to tell the system whether it can forward packets or not.
    
    Rationale: Setting the flag to 0 ensures that a system with multiple interfaces (for example, a hard proxy), will never be able to forward packets, and therefore, never serve as a router.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.ip_forward') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv4.conf.all.forwarding') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.1_Ensure_source_routed_packets_are_not_accepted" do
  title "Ensure source routed packets are not accepted"
  desc  "
    In networking, source routing allows a sender to partially or fully specify the route packets take through a network. In contrast, non-source routed packets travel a path determined by routers in the network. In some cases, systems may not be routable or reachable from some locations (e.g. private addresses vs. Internet routable), and so source routed packets would need to be used.
    
    Rationale: Setting net.ipv4.conf.all.accept_source_route and net.ipv4.conf.default.accept_source_route to 0 disables the system from accepting source routed packets. Assume this system was capable of routing packets to Internet routable addresses on one interface and private addresses on another interface. Assume that the private addresses were not routable to the Internet routable addresses and vice versa. Under normal routing circumstances, an attacker from the Internet routable addresses could not use the system as a way to reach the private address systems. If, however, source routed packets were allowed, they could be used to gain access to the private address systems as the route could be specified, rather than rely on routing protocols that did not allow this routing.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.conf.all.accept_source_route') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv4.conf.default.accept_source_route') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.3.1_Ensure_IPv6_router_advertisements_are_not_accepted" do
  title "Ensure IPv6 router advertisements are not accepted"
  desc  "
    This setting disables the system's ability to accept IPv6 router advertisements.
    
    Rationale: It is recommended that systems not accept router advertisements as they could be tricked into routing traffic to compromised machines. Setting hard routes within the system (usually a single default route to a trusted router) protects the system from bad routes.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.3.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv6.conf.all.accept_ra') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv6.conf.default.accept_ra') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.4.1_Ensure_TCP_Wrappers_is_installed" do
  title "Ensure TCP Wrappers is installed"
  desc  "
    TCP Wrappers provides a simple access list and standardized logging method for services capable of supporting it. In the past, services that were called from inetd and xinetd supported the use of tcp wrappers. As inetd and xinetd have been falling in disuse, any service that can support tcp wrappers will have the libwrap.so library attached to it.
    
    Rationale: TCP Wrappers provide a good simple access list mechanism to services that may not have that support built in. It is recommended that all services that can support TCP Wrappers, use it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.4.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('tcp_wrappers') do
    it { should be_installed }
  end
  describe package('tcp_wrappers-libs') do
    it { should be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.5.1_Ensure_DCCP_is_disabled" do
  title "Ensure DCCP is disabled"
  desc  "
    The Datagram Congestion Control Protocol (DCCP) is a transport layer protocol that supports streaming media and telephony. DCCP provides a way to gain access to congestion control, without having to do it at the application layer, but does not provide in-sequence delivery.
    
    Rationale: If the protocol is not required, it is recommended that the drivers not be installed to reduce the potential attack surface.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "3.5.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('dccp') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.6.1_Ensure_iptables_is_installed" do
  title "Ensure iptables is installed"
  desc  "
    iptables allows configuration of the IPv4 tables in the linux kernel and the rules stored within them. Most firewall configuration utilities operate as a front end to iptables.
    
    Rationale: iptables is required for firewall management and configuration.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.6.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('iptables') do
    it { should be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.3_Ensure_logrotate_is_configured" do
  title "Ensure logrotate is configured"
  desc  "
    The system includes the capability of rotating log files regularly to avoid filling up the system with logs or making the logs unmanageable large. The file /etc/logrotate.d/syslog is the configuration file used to rotate log files created by syslog or rsyslog .
    
    Rationale: By keeping the log files smaller and more manageable, a system administrator can easily archive these files to another system and spend less time looking through inordinately large log files.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": 4.3
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'Ensure logrotate' do
    skip 'Review /etc/logrotate.conf and /etc/logrotate.d/ * and verify logs are rotated according to site policy.'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.3_Ensure_rsyslog_or_syslog-ng_is_installed" do
  title "Ensure rsyslog or syslog-ng is installed"
  desc  "
    The rsyslog and syslog-ng software are recommended replacements to the original syslogd daemon which provide improvements over syslogd , such as connection-oriented (i.e. TCP) transmission of logs, the option to log to database formats, and the encryption of log data en route to a central logging server.
    
    Rationale: The security enhancements of rsyslog and syslog-ng such as connection-oriented (i.e. TCP) transmission of logs, the option to log to database formats, and the encryption of log data en route to a central logging server) justify installing and configuring the package.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe.one do
    describe package("rsyslog") do
      it { should be_installed }
    end
    describe package("syslog-ng") do
      it { should be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.1.1_Ensure_rsyslog_Service_is_enabled" do
  title "Ensure rsyslog Service is enabled"
  desc  "
    Once the rsyslog package is installed it needs to be activated.
    
    Rationale: If the rsyslog service is not activated the system may default to the syslogd service or lack logging instead.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('rsyslog') do
    it { should be_installed }
  end
  describe service('rsyslog') do
    it { should be_running }
    it { should be_enabled }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.2.1_Ensure_syslog-ng_service_is_enabled" do
  title "Ensure syslog-ng service is enabled"
  desc  "
    Once the syslog-ng package is installed it needs to be activated.
    
    Rationale: If the syslog-ng service is not activated the system may default to the syslogd service or lack logging instead.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.2.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('syslog-ng') do
    it { should be_enabled }
    it { should be_running }
  end
  describe package('syslog-ng') do
    it { should be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.5_Ensure_root_login_is_restricted_to_system_console" do
  title "Ensure root login is restricted to system console"
  desc  "
    The file /etc/securetty contains a list of valid terminals that may be logged in directly as root.
    
    Rationale: Since the system console has special properties to handle emergency situations, it is important to ensure that the console is in a physically secure location and that unauthorized consoles have not been defined.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": 5.5
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'system console' do
    skip 'Review the file /etc/securetty and remove entries for any consoles that are not in a physically secure location.'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.1_Ensure_cron_daemon_is_enabled" do
  title "Ensure cron daemon is enabled"
  desc  "
    The cron daemon is used to execute batch jobs on the system.
    
    Rationale: While there may not be user jobs that need to be run on the system, the system does have maintenance jobs that may include security monitoring that have to run, and cron is used to execute them.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('crond') do
    it { should be_enabled }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.1_Ensure_permissions_on_etcsshsshd_config_are_configured" do
  title "Ensure permissions on /etc/ssh/sshd_config are configured"
  desc  "
    The /etc/ssh/sshd_config file contains configuration specifications for sshd . The command below sets the owner and group of the file to root.
    
    Rationale: The /etc/ssh/sshd_config file needs to be protected from unauthorized changes by non-privileged users, but needs to be readable as this information is used with many non-privileged programs.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/ssh/sshd_config') do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should_not be_executable }
    it { should be_readable.by('owner') }
    it { should_not be_readable.by('group') }
    it { should_not be_readable.by('other') }
    it { should be_writable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_writable.by('other') }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.3.1_Ensure_password_creation_requirements_are_configured" do
  title "Ensure password creation requirements are configured"
  desc  "
    The pam_pwquality.so module checks the strength of passwords. It performs checks such as making sure a password is not a dictionary word, it is a certain length, contains a mix of characters (e.g. alphabet, numeric, other) and more. The following are definitions of the pam_pwquality .so options.
    
    * try_first_pass - retrieve the password from a previous stacked PAM module. If not available, then prompt the user for a password.
    * retry=3 - Allow 3 tries before sending back a failure.
    The following options are set in the /etc/security/pwquality.conf file:
    
    * minlen=14 - password must be 14 characters or more
    * dcredit=-1 - provide at least one digit
    * ucredit=-1 - provide at least one uppercase character
    * ocredit=-1 - provide at least one special character
    * lcredit=-1 - provide at least one lowercase character
    The settings shown above are one possible policy. Alter these values to conform to your own organization's password policies.
    
    Rationale: Strong passwords protect systems from being hacked through brute force methods.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.3.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/pam.d/system-auth") do
    its("content") { should match(/^\s*password\s+requisite\s+pam_pwquality\.so\s+(\S+\s+)*try_first_pass\s+(\S+\s+)*retry\=3\s*(\S+\s+)*$/) }
  end
  describe file("/etc/pam.d/password-auth") do
    its("content") { should match(/^\s*password\s+requisite\s+pam_pwquality\.so\s+(\S+\s+)*try_first_pass\s+(\S+\s+)*retry\=3\s*(\S+\s+)*$/) }
  end
  describe parse_config_file('/etc/security/pwquality.conf') do
    its('minlen') { should eq '14' }
    its('dcredit') { should eq '-1' }
    its('ucredit') { should eq '-1' }
    its('ocredit') { should eq '-1' }
    its('lcredit') { should eq '-1' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.2_Ensure_system_accounts_are_non-login" do
  title "Ensure system accounts are non-login"
  desc  "
    There are a number of accounts provided with Red Hat 7 that are used to manage applications and are not intended to provide an interactive shell.
    
    Rationale: It is important to make sure that accounts that are not being used by regular users are prevented from being used to provide an interactive shell. By default Red Hat 7 sets the password field for these accounts to an invalid string, but it is also recommended that the shell field in the password file be set to /sbin/nologin . This prevents the account from potentially being used to run any commands.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe passwd.where { user =~ /^(?!root|sync|shutdown|halt).*$/ } do
    its("entries") { should_not be_empty }
  end
  describe passwd.where { user =~ /^(?!root|sync|shutdown|halt).*$/ && uid.to_i < 1000 && shell != "/sbin/nologin" } do
    its("entries") { should be_empty }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.1.1_Ensure_password_expiration_is_90_days_or_less" do
  title "Ensure password expiration is 90 days or less"
  desc  "
    The PASS_MAX_DAYS parameter in /etc/login.defs allows an administrator to force passwords to expire once they reach a defined age. It is recommended that the PASS_MAX_DAYS parameter be set to less than or equal to 90 days.
    
    Rationale: The window of opportunity for an attacker to leverage compromised credentials or successfully compromise credentials via an online brute force attack is limited by the age of the password. Therefore, reducing the maximum age of a password also reduces an attacker's window of opportunity.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.1.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe login_defs do
    its('PASS_MAX_DAYS') { should eq '90' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.1_Ensure_password_fields_are_not_empty" do
  title "Ensure password fields are not empty"
  desc  "
    An account with an empty password field means that anybody may log in as that user without providing a password.
    
    Rationale: All accounts must have passwords or be locked to prevent the account from being used by an unauthorized user.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.1"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  shadow.users(/.+/).entries.each do |entry|
    describe entry do
      its('passwords') { should_not eq [''] }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.3_Ensure_nodev_option_set_on_tmp_partition" do
  title "Ensure nodev option set on /tmp partition"
  desc  "
    The nodev mount option specifies that the filesystem cannot contain special devices.
    
    Rationale: Since the /tmp filesystem is not intended to support devices, set this option to ensure that users cannot attempt to create block or character special devices in /tmp .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/tmp') do
    its('options') { should include 'nodev' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.2_Ensure_mounting_of_freevxfs_filesystems_is_disabled" do
  title "Ensure mounting of freevxfs filesystems is disabled"
  desc  "
    The freevxfs filesystem type is a free version of the Veritas type filesystem. This is the primary filesystem type for HP-UX operating systems.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('freevxfs') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.2.2_Ensure_gpgcheck_is_globally_activated" do
  title "Ensure gpgcheck is globally activated"
  desc  "
    The gpgcheck option, found in the main section of the /etc/yum.conf and individual /etc/yum/repos.d/* files determines if an RPM package's signature is checked prior to its installation.
    
    Rationale: It is important to ensure that an RPM's package signature is always checked prior to installation to ensure that the software is obtained from a trusted source.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.2.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe ini('/etc/yum.conf').params['main'] do
    its('gpgcheck') {should eq '1'}
  end
  repos = command('ls /etc/yum.repos.d').stdout.split("\n")
  repos.each do |repo|
    repo_list = ini("/etc/yum.repos.d/#{repo}").params
    repo_list.each do |single|
      describe repo_list[single[0]] do
        its('gpgcheck') {should eq '1'}
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.3.2_Ensure_filesystem_integrity_is_regularly_checked" do
  title "Ensure filesystem integrity is regularly checked"
  desc  "
    Periodic checking of the filesystem integrity is needed to detect changes to the filesystem.
    
    Rationale: Periodic file checking allows the system administrator to determine on a regular basis if critical files have been changed in an unauthorized fashion.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.3.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe.one do
    describe crontab do
      its('commands') { should include '/usr/sbin/aide --check' }
    end
    crontab_path = ['/etc/cron.hourly/', '/etc/cron.daily/', '/etc/cron.weekly/', '/etc/cron.monthly/', '/etc/cron.d/']
    all_cron_files = Hash.new
    crontab_path.map { |path| all_cron_files[path] = command("ls #{path}").stdout.split("\n") }
    all_cron_files.each do |cron_path, cron_files|
      unless cron_files.empty?
        cron_files.each do |cron_file|
          temp = file(cron_path+cron_file)
          describe temp do
            its('content') { should include 'aide --check'}
          end
        end
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.4.2_Ensure_bootloader_password_is_set" do
  title "Ensure bootloader password is set"
  desc  "
    Setting the boot loader password will require that anyone rebooting the system must enter a password before being able to set command line boot parameters
    
    Rationale: Requiring a boot password upon execution of the boot loader will prevent an unauthorized user from entering boot parameters or changing the boot partition. This prevents users from weakening security (e.g. turning off SELinux at boot time).
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.4.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/boot/grub2/grub.cfg") do
    its("content") { should match(/^\s*set\s+superusers\s*=\s*"[^"]*"\s*(\s+#.*)?$/) }
    its("content") { should match(/^\s*password_pbkdf2\s+\S+\s+\S+\s*(\s+#.*)?$/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.5.2_Ensure_XDNX_support_is_enabled" do
  title "Ensure XD/NX support is enabled"
  desc  "
    Recent processors in the x86 family support the ability to prevent code execution on a per memory page basis. Generically and on AMD processors, this ability is called No Execute (NX), while on Intel processors it is called Execute Disable (XD). This ability can help prevent exploitation of buffer overflow vulnerabilities and should be activated whenever possible. Extra steps must be taken to ensure that this protection is enabled, particularly on 32-bit x86 systems. Other processors, such as Itanium and POWER, have included such support since inception and the standard kernel for those platforms supports the feature.
    
    Rationale: Enabling any feature that can protect against buffer overflow attacks enhances the security of the system.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.5.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  flags = parse_config_file('/proc/cpuinfo', assignment_regex: /^([^:]*?)\s+:\s+(.*?)$/).flags
    flags ||= ''
    flags = flags.split(' ')
    describe '/proc/cpuinfo' do
      it 'Flags should include NX' do
        expect(flags).to include('nx')
      end
    end
    unless flags.include?('nx')
      # if no nx flag is present, we require exec-shield
      describe kernel_parameter('kernel.exec-shield') do
        its('value') { should eq 1 }
      end
    end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.1.2_Ensure_local_login_warning_banner_is_configured_properly" do
  title "Ensure local login warning banner is configured properly"
  desc  "
    The contents of the /etc/issue file are displayed to users prior to login for local terminals.
    
    Unix-based systems have typically displayed information about the OS release and patch level upon logging in to the system. This information can be useful to developers who are developing software for a particular OS platform. If mingetty(8) supports the following options, they display operating system information: \\m - machine architecture ( uname -m ) \\r - operating system release ( uname -r ) \\s - operating system name \\v - operating system version ( uname -v )
    
    Rationale: Warning messages inform users who are attempting to login to the system of their legal status regarding the system and must include the name of the organization that owns the system and any monitoring policies that are in place. Displaying OS and patch level information in login banners also has the side effect of providing detailed system information to attackers attempting to target specific exploits of a system. Authorized users can easily get this information by running the \" uname -a \" command once they have logged in.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.7.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/issue") do
    its("content") { should_not match(/(\\v|\\r|\\m|\\s)/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.2_Ensure_daytime_services_are_not_enabled" do
  title "Ensure daytime services are not enabled"
  desc  "
    daytime is a network service that responds with the server's current date and time. This service is intended for debugging and testing purposes. It is recommended that this service be disabled.
    
    Rationale: Disabling this service will reduce the remote attack surface of the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  if package('xinetd').installed?
    describe xinetd_conf.services('daytime') do
      it { should be_disabled }
    end
  else
    describe package('xinetd') do
      it { should_not be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.3_Ensure_Avahi_Server_is_not_enabled" do
  title "Ensure Avahi Server is not enabled"
  desc  "
    Avahi is a free zeroconf implementation, including a system for multicast DNS/DNS-SD service discovery. Avahi allows programs to publish and discover services and hosts running on a local network with no specific configuration. For example, a user can plug a computer into a network and Avahi automatically finds printers to print to, files to look at and people to talk to, as well as network services running on the machine.
    
    Rationale: Automatic discovery of network services is not normally required for system functionality. It is recommended to disable the service to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('avahi-daemon') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.1.2_Ensure_ntp_is_configured" do
  title "Ensure ntp is configured"
  desc  "
    ntp is a daemon which implements the Network Time Protocol (NTP). It is designed to synchronize system clocks across a variety of systems and use a source that is highly accurate. More information on NTP can be found at [http://www.ntp.org](http://www.ntp.org/). ntp can be configured to be a client and/or a server.
    
    This recommendation only applies if ntp is in use on the system.
    
    Rationale: If ntp is in use on the system proper configuration is vital to ensuring time synchronization is working properly.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ntp_restrict = (ntp_conf.restrict || []).map { |value| ((value.strip =~ Resolv::IPv4::Regex) ? nil : value) && ((value =~ Resolv::IPv6::Regex) ? nil : value) }.compact
  unless ntp_restrict == nil
    ntp_restrict.each do |value|
      describe value do
        it { should include 'default' }
        it { should include 'kod' }
        it { should include 'notrap' }
        it { should include 'nomodify' }
        it { should include 'nopeer' }
        it { should include 'noquery' }
      end
    end
  end
  describe ntp_conf.server do
    it { should eq NTP_SERVER }
  end
  describe ntp_conf do
    its('server') { should eq NTP_SERVER }
  end
  opts = ini('/etc/sysconfig/ntpd').params['OPTIONS'] || []
  if opts.include?('-u ntp:ntp')
    describe ini('/etc/sysconfig/ntpd') do
      its('OPTIONS') { should include '-u ntp:ntp' }
    end
  else
    describe ini('/usr/lib/systemd/system/ntpd.service') do
      its(['Service', 'ExecStart']) { should include '-u ntp:ntp' }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.3.2_Ensure_rsh_client_is_not_installed" do
  title "Ensure rsh client is not installed"
  desc  "
    The rsh package contains the client commands for the rsh services.
    
    Rationale: These legacy clients contain numerous security exposures and have been replaced with the more secure SSH package. Even if the server is removed, it is best to ensure the clients are also removed to prevent users from inadvertently attempting to use these commands and therefore exposing their credentials. Note that removing the rsh package removes the clients for rsh , rcp and rlogin .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.3.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('rsh') do
    it { should_not be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.1.2_Ensure_packet_redirect_sending_is_disabled" do
  title "Ensure packet redirect sending is disabled"
  desc  "
    ICMP Redirects are used to send routing information to other hosts. As a host itself does not act as a router (in a host only configuration), there is no need to send redirects.
    
    Rationale: An attacker could use a compromised host to send invalid ICMP redirects to other router devices in an attempt to corrupt routing and have users access a system set up by the attacker as opposed to a valid system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.conf.default.accept_redirects') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv4.conf.all.accept_redirects') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.2_Ensure_ICMP_redirects_are_not_accepted" do
  title "Ensure ICMP redirects are not accepted"
  desc  "
    ICMP redirect messages are packets that convey routing information and tell your host (acting as a router) to send packets via an alternate path. It is a way of allowing an outside routing device to update your system routing tables. By setting net.ipv4.conf.all.accept_redirects to 0, the system will not accept any ICMP redirect messages, and therefore, won't allow outsiders to update the system's routing tables.
    
    Rationale: Attackers could use bogus ICMP redirect messages to maliciously alter the system routing tables and get them to send packets to incorrect networks and allow your system packets to be captured.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.conf.default.accept_redirects') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv4.conf.all.accept_redirects') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.3.2_Ensure_IPv6_redirects_are_not_accepted" do
  title "Ensure IPv6 redirects are not accepted"
  desc  "
    This setting prevents the system from accepting ICMP redirects. ICMP redirects tell the system about alternate routes for sending traffic.
    
    Rationale: It is recommended that systems not accept ICMP redirects as they could be tricked into routing traffic to compromised machines. Setting hard routes within the system (usually a single default route to a trusted router) protects the system from bad routes.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.3.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv6.conf.all.accept_redirects') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv6.conf.default.accept_redirects') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.4.2_Ensure_etchosts.allow_is_configured" do
  title "Ensure /etc/hosts.allow is configured"
  desc  "
    The /etc/hosts.allow file specifies which IP addresses are permitted to connect to the host. It is intended to be used in conjunction with the /etc/hosts.deny file.
    
    Rationale: The /etc/hosts.allow file supports access control by IP and helps ensure that only authorized systems can connect to the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.4.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/hosts.allow') do
    it { should exist }
    it { should be_file }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.5.2_Ensure_SCTP_is_disabled" do
  title "Ensure SCTP is disabled"
  desc  "
    The Stream Control Transmission Protocol (SCTP) is a transport layer protocol used to support message oriented communication, with several streams of messages in one connection. It serves a similar function as TCP and UDP, incorporating features of both. It is message-oriented like UDP, and ensures reliable in-sequence transport of messages with congestion control like TCP.
    
    Rationale: If the protocol is not being used, it is recommended that kernel module not be loaded, disabling the service to reduce the potential attack surface.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "3.5.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('sctp') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.6.2_Ensure_default_deny_firewall_policy" do
  title "Ensure default deny firewall policy"
  desc  "
    A default deny all policy on connections ensures that any unconfigured network usage will be rejected.
    
    Rationale: With a default accept policy the firewall will accept any packet that is not configured to be denied. It is easier to white list acceptable usage than to black list unacceptable usage.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.6.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  %w[INPUT OUTPUT FORWARD].each do |chain|
    describe.one do
      describe iptables do
        it { should have_rule("-P #{chain} DROP") }
      end
      describe iptables do
        it { should have_rule("-P #{chain} REJECT") }
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.4_Ensure_permissions_on_all_logfiles_are_configured" do
  title "Ensure permissions on all logfiles are configured"
  desc  "
    Log files stored in /var/log/ contain logged information from many services on the system, or on log hosts others as well.
    
    Rationale: It is important to ensure that log files have the correct permissions to ensure that sensitive data is archived and protected.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  command('find /var/log -type f').stdout.split("\n").each do |log_file|
    describe file(log_file) do
      it { should_not be_writable.by('group') }
      it { should_not be_executable.by('group') }
      it { should_not be_readable.by('other') }
      it { should_not be_writable.by('other') }
      it { should_not be_executable.by('other') }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.1.2_Ensure_logging_is_configured" do
  title "Ensure logging is configured"
  desc  "
    The /etc/rsyslog.conf file specifies rules for logging and which files are to be used to log certain classes of messages.
    
    Rationale: A great deal of important security-related information is sent via rsyslog (e.g., successful and failed su attempts, failed login attempts, root login attempts, etc.).
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "4.2.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'Ensure logging' do
    skip 'Review the contents of the /etc/rsyslog.conf file to ensure appropriate logging is set. In addition, run the following command and verify that the log files are logging information: ls -la /var/log/'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.2.2_Ensure_logging_is_configured" do
  title "Ensure logging is configured"
  desc  "
    The /etc/syslog-ng/syslog-ng.conf file specifies rules for logging and which files are to be used to log certain classes of messages.
    
    Rationale: A great deal of important security-related information is sent via syslog-ng (e.g., successful and failed su attempts, failed login attempts, root login attempts, etc.).
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "4.2.2.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'syslog-ng config' do
    skip 'Edit the log lines in the /etc/syslog-ng/syslog-ng.conf file as appropriate for your environment.'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.6_Ensure_access_to_the_su_command_is_restricted" do
  title "Ensure access to the su command is restricted"
  desc  "
    The su command allows a user to run a command or shell as another user. The program has been superseded by sudo , which allows for more granular control over privileged access. Normally, the su command can be executed by any user. By uncommenting the pam_wheel.so statement in /etc/pam.d/su , the su command will only allow users in the wheel group to execute su .
    
    Rationale: Restricting the use of su , and using sudo in its place, provides system administrators better control of the escalation of user privileges to execute privileged commands. The sudo utility also provides a better logging and audit mechanism, as it can log each command executed via sudo , whereas su can only record that a user executed the su program.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": 5.6
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/pam.d/su") do
    its("content") { should match(/^\s*auth\s+required\s+pam_wheel.so\s+(\S+\s+)*use_uid\s*(\S+\s+)*$/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.2_Ensure_permissions_on_etccrontab_are_configured" do
  title "Ensure permissions on /etc/crontab are configured"
  desc  "
    The /etc/crontab file is used by cron to control its own jobs. The commands in this item make sure that root is the user and group owner of the file and that only the owner can access the file.
    
    Rationale: This file contains information on what system jobs are run by cron. Write access to these files could provide unprivileged users with the ability to elevate their privileges. Read access to these files could provide users with the ability to gain insight on system jobs that run on the system and could provide them a way to gain unauthorized privileged access.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/crontab') do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.2_Ensure_SSH_Protocol_is_set_to_2" do
  title "Ensure SSH Protocol is set to 2"
  desc  "
    SSH supports two different and incompatible protocols: SSH1 and SSH2. SSH1 was the original protocol and was subject to security issues. SSH2 is more advanced and secure.
    
    Rationale: SSH v1 suffers from insecurities that do not affect SSH v2.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('Protocol') { should cmp 2 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.3.2_Ensure_lockout_for_failed_password_attempts_is_configured" do
  title "Ensure lockout for failed password attempts is configured"
  desc  "
    Lock out users after **n** unsuccessful consecutive login attempts. The first sets of changes are made to the PAM configuration files. The second set of changes are applied to the program specific PAM configuration file. The second set of changes must be applied to each program that will lock out users. Check the documentation for each secondary program for instructions on how to configure them to work with PAM.
    
    Set the lockout number to the policy in effect at your site.
    
    Rationale: Locking out user IDs after **n** unsuccessful consecutive login attempts mitigates brute force password attacks against your systems.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.3.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/pam.d/system-auth") do
    its("content") { should match(/^\s*auth\s+required\s+pam_faillock\.so\s+(\S+\s+)*preauth\s+(\S+\s+)*audit\s+(\S+\s+)*silent\s+(\S+\s+)*deny\=5\s+(\S+\s+)*unlock_time\=900\s*(\S+\s+)*$/) }
    its("content") { should match(/^\s*auth\s+\[success\=1\s+default\=bad]\s+pam\_unix\.so\s*(\S+\s+)*$/) }
    its("content") { should match(/^\s*auth\s+\[default\=die\]\s+pam_faillock\.so\s+(\S+\s+)*authfail\s+(\S+\s+)*audit\s+(\S+\s+)*deny\=5\s+(\S+\s+)*unlock\_time\=900\s*(\S+\s+)*$/) }
    its("content") { should match(/^\s*auth\s+sufficient\s+pam_faillock\.so\s+(\S+\s+)*authsucc\s+(\S+\s+)*audit\s+(\S+\s+)*deny\=5\s+(\S+\s+)*unlock\_time\=900\s*(\S+\s+)*$/) }
  end
  describe file("/etc/pam.d/password-auth") do
    its("content") { should match(/^\s*auth\s+required\s+pam_faillock\.so\s+(\S+\s+)*preauth\s+(\S+\s+)*audit\s+(\S+\s+)*silent\s+(\S+\s+)*deny\=5\s+(\S+\s+)*unlock_time\=900\s*(\S+\s+)*$/) }
    its("content") { should match(/^\s*auth\s+\[success\=1\s+default\=bad]\s+pam\_unix\.so\s*(\S+\s+)*$/) }
    its("content") { should match(/^\s*auth\s+\[default\=die\]\s+pam_faillock\.so\s+(\S+\s+)*authfail\s+(\S+\s+)*audit\s+(\S+\s+)*deny\=5\s+(\S+\s+)*unlock\_time\=900\s*(\S+\s+)*$/) }
    its("content") { should match(/^\s*auth\s+sufficient\s+pam_faillock\.so\s+(\S+\s+)*authsucc\s+(\S+\s+)*audit\s+(\S+\s+)*deny\=5\s+(\S+\s+)*unlock\_time\=900\s*(\S+\s+)*$/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.3_Ensure_default_group_for_the_root_account_is_GID_0" do
  title "Ensure default group for the root account is GID 0"
  desc  "
    The usermod command can be used to specify which group the root user belongs to. This affects permissions of files that are created by the root user.
    
    Rationale: Using GID 0 for the root account helps prevent root -owned files from accidentally becoming accessible to non-privileged users.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe passwd.users('root') do
    its('gids') { should cmp 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.1.2_Ensure_minimum_days_between_password_changes_is_7_or_more" do
  title "Ensure minimum days between password changes is 7 or more"
  desc  "
    The PASS_MIN_DAYS parameter in /etc/login.defs allows an administrator to prevent users from changing their password until a minimum number of days have passed since the last time the user changed their password. It is recommended that PASS_MIN_DAYS parameter be set to 7 or more days.
    
    Rationale: By restricting the frequency of password changes, an administrator can prevent users from repeatedly changing their password in an attempt to circumvent password reuse controls.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe login_defs do
    its('PASS_MIN_DAYS') { should cmp >=7 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.2_Ensure_permissions_on_etcpasswd_are_configured" do
  title "Ensure permissions on /etc/passwd are configured"
  desc  "
    The /etc/passwd file contains user account information that is used by many system utilities and therefore must be readable for these utilities to operate.
    
    Rationale: It is critical to ensure that the /etc/passwd file is protected from unauthorized write access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/passwd") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.2_Ensure_no_legacy__entries_exist_in_etcpasswd" do
  title "Ensure no legacy \"+\" entries exist in /etc/passwd"
  desc  "
    The character + in various files used to be markers for systems to insert data from NIS maps at a certain point in a system configuration file. These entries are no longer required on most systems, but may exist in files that have been imported from other platforms.
    
    Rationale: These entries may provide an avenue for attackers to gain privileged access on the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.2"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe passwd do
    its('users') { should_not include '+' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.4_Ensure_nosuid_option_set_on_tmp_partition" do
  title "Ensure nosuid option set on /tmp partition"
  desc  "
    The nosuid mount option specifies that the filesystem cannot contain setuid files.
    
    Rationale: Since the /tmp filesystem is only intended for temporary file storage, set this option to ensure that users cannot create setuid files in /tmp .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/tmp') do
    its('options') { should include 'nosuid' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.3_Ensure_mounting_of_jffs2_filesystems_is_disabled" do
  title "Ensure mounting of jffs2 filesystems is disabled"
  desc  "
    The jffs2 (journaling flash filesystem 2) filesystem type is a log-structured filesystem used in flash memory devices.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('jffs2') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.2.3_Ensure_GPG_keys_are_configured" do
  title "Ensure GPG keys are configured"
  desc  "
    Most packages managers implement GPG key signing to verify package integrity during installation.
    
    Rationale: It is important to ensure that updates are obtained from a valid source to protect against spoofing that could lead to the inadvertent installation of malware on the system.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  cmd = command('rpm -q gpg-pubkey').stdout.split("\n")
  diff = cmd - REDHAT_PUBKEY
  describe diff do
    it { should eq [] }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.4.3_Ensure_authentication_required_for_single_user_mode" do
  title "Ensure authentication required for single user mode"
  desc  "
    Single user mode (rescue mode) is used for recovery when the system detects an issue during boot or by manual selection from the bootloader.
    
    Rationale: Requiring authentication in single user mode (rescue mode) prevents an unauthorized user from rebooting the system into single user to gain root privileges without credentials.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.4.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe ini('/usr/lib/systemd/system/emergency.service') do
    its(['Service', 'ExecStart']) { should eq '-/bin/sh -c "/usr/sbin/sulogin; /usr/bin/systemctl --fail --no-block default"' }
  end
  describe ini('/usr/lib/systemd/system/rescue.service') do
    its(['Service', 'ExecStart']) { should eq '-/bin/sh -c "/usr/sbin/sulogin; /usr/bin/systemctl --fail --no-block default"' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.5.3_Ensure_address_space_layout_randomization_ASLR_is_enabled" do
  title "Ensure address space layout randomization (ASLR) is enabled"
  desc  "
    Address space layout randomization (ASLR) is an exploit mitigation technique which randomly arranges the address space of key data areas of a process.
    
    Rationale: Randomly placing virtual memory regions will make it difficult to write memory page exploits as the memory placement will be consistently shifting.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.5.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('kernel.randomize_va_space') do
    its('value') { should eq 2 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.1.3_Ensure_remote_login_warning_banner_is_configured_properly" do
  title "Ensure remote login warning banner is configured properly"
  desc  "
    The contents of the /etc/issue.net file are displayed to users prior to login for remote connections from configured services.
    
    Unix-based systems have typically displayed information about the OS release and patch level upon logging in to the system. This information can be useful to developers who are developing software for a particular OS platform. If mingetty(8) supports the following options, they display operating system information: \\m - machine architecture ( uname -m ) \\r - operating system release ( uname -r ) \\s - operating system name \\v - operating system version ( uname -v )
    
    Rationale: Warning messages inform users who are attempting to login to the system of their legal status regarding the system and must include the name of the organization that owns the system and any monitoring policies that are in place. Displaying OS and patch level information in login banners also has the side effect of providing detailed system information to attackers attempting to target specific exploits of a system. Authorized users can easily get this information by running the \" uname -a \" command once they have logged in.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.7.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/issue.net") do
    its("content") { should_not match(/(\\v|\\r|\\m|\\s)/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.3_Ensure_discard_services_are_not_enabled" do
  title "Ensure discard services are not enabled"
  desc  "
    discardis a network service that simply discards all data it receives. This service is intended for debugging and testing purposes. It is recommended that this service be disabled.
    
    Rationale: Disabling this service will reduce the remote attack surface of the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  if package('xinetd').installed?
    describe xinetd_conf.services('discard') do
      it { should be_disabled }
    end
  else
    describe package('xinetd') do
      it { should_not be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.4_Ensure_CUPS_is_not_enabled" do
  title "Ensure CUPS is not enabled"
  desc  "
    The Common Unix Print System (CUPS) provides the ability to print to both local and network printers. A system running CUPS can also accept print jobs from remote systems and print them to local printers. It also provides a web based remote administration capability.
    
    Rationale: If the system does not need to print jobs or accept print jobs from other systems, it is recommended that CUPS be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('cups') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.1.3_Ensure_chrony_is_configured" do
  title "Ensure chrony is configured"
  desc  "
    chrony is a daemon which implements the Network Time Protocol (NTP) is designed to synchronize system clocks across a variety of systems and use a source that is highly accurate. More information on chrony can be found at [http://chrony.tuxfamily.org/](http://chrony.tuxfamily.org/). chrony can be configured to be a client and/or a server.
    
    Rationale: If chrony is in use on the system proper configuration is vital to ensuring time synchronization is working properly.
    
    This recommendation only applies if chrony is in use on the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  options = {
    assignment_regex: /^\s*(\S+)\s+(.*)\s*$/, multiple_values: true
  }
  chrony_conf = parse_config_file('/etc/chrony.conf', options).params
  describe parse_config_file('/etc/chrony.conf', options).params['server'] do
    it { should eq NTP_SERVER }
  end
  opts = ini('/etc/sysconfig/ntpd').params['OPTIONS'] || []
  if opts.include?('-u chrony')
    describe ini('/etc/sysconfig/chronyd') do
      its('OPTIONS') { should include '-u chrony' }
    end
  else
    describe ini('/usr/lib/systemd/system/chronyd.service') do
      its(['Service', 'ExecStart']) { should include '-u chrony' }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.3.3_Ensure_talk_client_is_not_installed" do
  title "Ensure talk client is not installed"
  desc  "
    The talk software makes it possible for users to send and receive messages across systems through a terminal session. The talk client, which allows initialization of talk sessions, is installed by default.
    
    Rationale: The software presents a security risk as it uses unencrypted protocols for communication.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.3.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('talk') do
    it { should_not be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.3_Ensure_secure_ICMP_redirects_are_not_accepted" do
  title "Ensure secure ICMP redirects are not accepted"
  desc  "
    Secure ICMP redirects are the same as ICMP redirects, except they come from gateways listed on the default gateway list. It is assumed that these gateways are known to your system, and that they are likely to be secure.
    
    Rationale: It is still possible for even known gateways to be compromised. Setting net.ipv4.conf.all.secure_redirects to 0 protects the system from routing table updates by possibly compromised known gateways.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.conf.all.secure_redirects') do
    its('value') { should eq 0 }
  end
  describe kernel_parameter('net.ipv4.conf.default.secure_redirects') do
    its('value') { should eq 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.3.3_Ensure_IPv6_is_disabled" do
  title "Ensure IPv6 is disabled"
  desc  "
    Although IPv6 has many advantages over IPv4, few organizations have implemented IPv6.
    
    Rationale: If IPv6 is not to be used, it is recommended that it be disabled to reduce the attack surface of the system.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "3.3.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"Disable ipv6 Red Hat 7 solution", :url=>"https://access.redhat.com/solutions/8709"})
  describe kernel_parameter('net.ipv6.conf.all.disable_ipv6') do
    its('value') { should eq 1 }
  end
  describe kernel_parameter('net.ipv6.conf.default.disable_ipv6') do
    its('value') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.4.3_Ensure_etchosts.deny_is_configured" do
  title "Ensure /etc/hosts.deny is configured"
  desc  "
    The /etc/hosts.deny file specifies which IP addresses are **not** permitted to connect to the host. It is intended to be used in conjunction with the /etc/hosts.allow file.
    
    Rationale: The /etc/hosts.deny file serves as a failsafe so that any host not specified in /etc/hosts.allow is denied access to the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.4.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/hosts.deny') do
    it { should exist }
    it { should be_file }
    its('content') { should match /^\s*ALL\:\s*ALL\s*$/}
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.5.3_Ensure_RDS_is_disabled" do
  title "Ensure RDS is disabled"
  desc  "
    The Reliable Datagram Sockets (RDS) protocol is a transport layer protocol designed to provide low-latency, high-bandwidth communications between cluster nodes. It was developed by the Oracle Corporation.
    
    Rationale: If the protocol is not being used, it is recommended that kernel module not be loaded, disabling the service to reduce the potential attack surface.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "3.5.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('rds') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.6.3_Ensure_loopback_traffic_is_configured" do
  title "Ensure loopback traffic is configured"
  desc  "
    Configure the loopback interface to accept traffic. Configure all other interfaces to deny traffic to the loopback network (127.0.0.0/8).
    
    Rationale: Loopback traffic is generated between processes on machine and is typically critical to operation of the system. The loopback interface is the only place that loopback network (127.0.0.0/8) traffic should be seen, all other interfaces should ignore traffic on this network as an anti-spoofing measure.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.6.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe iptables do
    it { should have_rule('-A INPUT -i lo -j ACCEPT') }
    it { should have_rule('-A OUTPUT -o lo -j ACCEPT') }
    it { should have_rule('-A INPUT -s 127.0.0.0/8 -j DROP') }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.1.3_Ensure_rsyslog_default_file_permissions_configured" do
  title "Ensure rsyslog default file permissions configured"
  desc  "
    rsyslog will create logfiles that do not already exist on the system. This setting controls what permissions will be applied to these newly created files.
    
    Rationale: It is important to ensure that log files have the correct permissions to ensure that sensitive data is archived and protected.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe parse_config_file('/etc/rsyslog.conf').params do
    it { should include ('$FileCreateMode 0640') }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.2.3_Ensure_syslog-ng_default_file_permissions_configured" do
  title "Ensure syslog-ng default file permissions configured"
  desc  "
    syslog-ng will create logfiles that do not already exist on the system. This setting controls what permissions will be applied to these newly created files.
    
    Rationale: It is important to ensure that log files exist and have the correct permissions to ensure that sensitive syslog-ng data is archived and protected.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe parse_config_file('/etc/syslog-ng/syslog-ng.conf').params do
    it { should include ('perm(0640);') }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.3_Ensure_permissions_on_etccron.hourly_are_configured" do
  title "Ensure permissions on /etc/cron.hourly are configured"
  desc  "
    This directory contains system cron jobs that need to run on an hourly basis. The files in this directory cannot be manipulated by the crontab command, but are instead edited by system administrators using a text editor. The commands below restrict read/write and search access to user and group root, preventing regular users from accessing this directory.
    
    Rationale: Granting write access to this directory for non-privileged users could provide them the means for gaining unauthorized elevated privileges. Granting read access to this directory could give an unprivileged user insight in how to gain elevated privileges or circumvent auditing controls.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/cron.hourly') do
    it { should exist }
    it { should be_directory }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.3_Ensure_SSH_LogLevel_is_set_to_INFO" do
  title "Ensure SSH LogLevel is set to INFO"
  desc  "
    The INFO parameter specifies that login and logout activity will be logged.
    
    Rationale: SSH provides several logging levels with varying amounts of verbosity. DEBUG is specifically **not** recommended other than strictly for debugging SSH communications since it provides so much data that it is difficult to identify important security information. INFO level is the basic level that only records login activity of SSH users. In many situations, such as Incident Response, it is important to determine when a particular user was active on a system. The logout record can eliminate those users who disconnected, which helps narrow the field.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('LogLevel') { should eq 'Info' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.3.3_Ensure_password_reuse_is_limited" do
  title "Ensure password reuse is limited"
  desc  "
    The /etc/security/opasswd file stores the users' old passwords and can be checked to ensure that users are not recycling recent passwords.
    
    Rationale: Forcing users not to reuse their past 5 passwords make it less likely that an attacker will be able to guess the password.
    
    Note that these change only apply to accounts configured on the local system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.3.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/pam.d/system-auth") do
    its("content") { should match(/^\s*password\s+sufficient\s+pam\_unix\.so\s+(\S+\s+)*remember=5\s*(\S+\s+)*$/) }
  end
  describe file("/etc/pam.d/password-auth") do
    its("content") { should match(/^\s*password\s+sufficient\s+pam\_unix\.so\s+(\S+\s+)*remember=5\s*(\S+\s+)*$/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.4_Ensure_default_user_umask_is_027_or_more_restrictive" do
  title "Ensure default user umask is 027 or more restrictive"
  desc  "
    The default umask determines the permissions of files created by users. The user creating the file has the discretion of making their files and directories readable by others via the chmod command. Users who wish to allow their files and directories to be readable by others by default may choose a different default umask by inserting the umask command into the standard shell configuration files ( .profile , .bashrc , etc.) in their home directories.
    
    Rationale: Setting a very secure default value for umask ensures that users make a conscious choice about their file permissions. A default umask setting of 077 causes files and directories created by users to not be readable by any other user on the system. A umask of 027 would make files and directories readable by users in the same Unix group, while a umask of 022 would make files readable by every user on the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/bashrc") do
    its("content") { should match(/^\s*umask\s+[01234567][234567]7\s*(\s+#.*)?$/) }
  end
  describe file("/etc/profile") do
    its("content") { should match(/^\s*umask\s+[01234567][234567]7\s*(\s+#.*)?$/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.1.3_Ensure_password_expiration_warning_days_is_7_or_more" do
  title "Ensure password expiration warning days is 7 or more"
  desc  "
    The PASS_WARN_AGE parameter in /etc/login.defs allows an administrator to notify users that their password will expire in a defined number of days. It is recommended that the PASS_WARN_AGE parameter be set to 7 or more days.
    
    Rationale: Providing an advance warning that a password will be expiring gives users time to think of a secure password. Users caught unaware may choose a simple password or write it down where it may be discovered.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe login_defs do
    its('PASS_WARN_AGE') { should cmp >=7 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.3_Ensure_permissions_on_etcshadow_are_configured" do
  title "Ensure permissions on /etc/shadow are configured"
  desc  "
    The /etc/shadow file is used to store the information about user accounts that is critical to the security of those accounts, such as the hashed password and other security information.
    
    Rationale: If attackers can gain read access to the /etc/shadow file, they can easily run a password cracking program against the hashed password to break it. Other security information that is stored in the /etc/shadow file (such as expiration) could also be useful to subvert the user accounts.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/shadow") do
    it { should exist }
    it { should be_file }
    it { should_not be_readable }
    it { should_not be_writable }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.3_Ensure_no_legacy__entries_exist_in_etcshadow" do
  title "Ensure no legacy \"+\" entries exist in /etc/shadow"
  desc  "
    The character + in various files used to be markers for systems to insert data from NIS maps at a certain point in a system configuration file. These entries are no longer required on most systems, but may exist in files that have been imported from other platforms.
    
    Rationale: These entries may provide an avenue for attackers to gain privileged access on the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.3"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe shadow do
    its('users') { should_not include '+' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.5_Ensure_noexec_option_set_on_tmp_partition" do
  title "Ensure noexec option set on /tmp partition"
  desc  "
    The noexec mount option specifies that the filesystem cannot contain executable binaries.
    
    Rationale: Since the /tmp filesystem is only intended for temporary file storage, set this option to ensure that users cannot run executable binaries from /tmp .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/tmp') do
    its('options') { should include 'noexec' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.4_Ensure_mounting_of_hfs_filesystems_is_disabled" do
  title "Ensure mounting of hfs filesystems is disabled"
  desc  "
    The hfs filesystem type is a hierarchical filesystem that allows you to mount Mac OS filesystems.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('hfs') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.2.4_Ensure_Red_Hat_Network_or_Subscription_Manager_connection_is_configured" do
  title "Ensure Red Hat Network or Subscription Manager connection is configured"
  desc  "
    Systems need to be registered with the Red Hat Network (RHN) or Red Hat Subscription Manager (RHSM) to receive patch updates. This is usually configured during initial installation.
    
    Rationale: It is important to register with the Red Hat Network to make sure that patches are updated on a regular basis. This helps to reduce the exposure time as new vulnerabilities are discovered.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  options = { assignment_regex: /^\s*([^:]*?)\s*:\s*(.*?)\s*$/ }
  describe parse_config(command('subscription\-manager identity').stdout,options) do
    its('system identity') { should_not include 'This system is not yet registered.' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.5.4_Ensure_prelink_is_disabled" do
  title "Ensure prelink is disabled"
  desc  "
    prelinkis a program that modifies ELF shared libraries and ELF dynamically linked binaries in such a way that the time needed for the dynamic linker to perform relocations at startup significantly decreases.
    
    Rationale: The prelinking feature can interfere with the operation of AIDE, because it changes binaries. Prelinking can also increase the vulnerability of the system if a malicious user is able to compromise a common library such as libc.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.5.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('prelink') do
    it { should_not be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.1.4_Ensure_permissions_on_etcmotd_are_configured" do
  title "Ensure permissions on /etc/motd are configured"
  desc  "
    The contents of the /etc/motd file are displayed to users after login and function as a message of the day for authenticated users.
    
    Rationale: If the /etc/motd file does not have the correct ownership it could be modified by unauthorized users with incorrect or misleading information.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.7.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/motd") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.4_Ensure_echo_services_are_not_enabled" do
  title "Ensure echo services are not enabled"
  desc  "
    echois a network service that responds to clients with the data sent to it by the client. This service is intended for debugging and testing purposes. It is recommended that this service be disabled.
    
    Rationale: Disabling this service will reduce the remote attack surface of the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  if package('xinetd').installed?
    describe xinetd_conf.services('echo') do
      it { should be_disabled }
    end
  else
    describe package('xinetd') do
      it { should_not be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.5_Ensure_DHCP_Server_is_not_enabled" do
  title "Ensure DHCP Server is not enabled"
  desc  "
    The Dynamic Host Configuration Protocol (DHCP) is a service that allows machines to be dynamically assigned IP addresses.
    
    Rationale: Unless a system is specifically set up to act as a DHCP server, it is recommended that this service be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('dhcpd') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.3.4_Ensure_telnet_client_is_not_installed" do
  title "Ensure telnet client is not installed"
  desc  "
    The telnet package contains the telnet client, which allows users to start connections to other systems via the telnet protocol.
    
    Rationale: The telnet protocol is insecure and unencrypted. The use of an unencrypted transmission medium could allow an unauthorized user to steal credentials. The ssh package provides an encrypted session and stronger security and is included in most Linux distributions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.3.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('telnet') do
    it { should_not be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.4_Ensure_suspicious_packets_are_logged" do
  title "Ensure suspicious packets are logged"
  desc  "
    When enabled, this feature logs packets with un-routable source addresses to the kernel log.
    
    Rationale: Enabling this feature and logging these packets allows an administrator to investigate the possibility that an attacker is sending spoofed packets to their system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.conf.all.log_martians') do
    its('value') { should eq 1 }
  end
  describe kernel_parameter('net.ipv4.conf.default.log_martians') do
    its('value') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.4.4_Ensure_permissions_on_etchosts.allow_are_configured" do
  title "Ensure permissions on /etc/hosts.allow are configured"
  desc  "
    The /etc/hosts.allow file contains networking information that is used by many applications and therefore must be readable for these applications to operate.
    
    Rationale: It is critical to ensure that the /etc/hosts.allow file is protected from unauthorized write access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.4.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/hosts.allow') do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.5.4_Ensure_TIPC_is_disabled" do
  title "Ensure TIPC is disabled"
  desc  "
    The Transparent Inter-Process Communication (TIPC) protocol is designed to provide communication between cluster nodes.
    
    Rationale: If the protocol is not being used, it is recommended that kernel module not be loaded, disabling the service to reduce the potential attack surface.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "3.5.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  tag "set-refs": [{"ref"=>"TIPC removed from Red Hat 7 reposioty", "url"=>"https://bugzilla.redhat.com/show_bug.cgi?id=574800"}]
  describe kernel_module('tipc') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.6.4_Ensure_outbound_and_established_connections_are_configured" do
  title "Ensure outbound and established connections are configured"
  desc  "
    Configure the firewall rules for new outbound, and established connections.
    
    Rationale: If rules are not in place for new outbound, and established connections all packets will be dropped by the default policy preventing network usage.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "3.6.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  unless IPTABLES_RULES.empty?
    IPTABLES_RULES.each do |rule|
      describe iptables do
        it { should have_rule(rule) }
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.1.4_Ensure_rsyslog_is_configured_to_send_logs_to_a_remote_log_host" do
  title "Ensure rsyslog is configured to send logs to a remote log host"
  desc  "
    The rsyslog utility supports the ability to send logs it gathers to a remote log host running syslogd(8) or to receive messages from remote hosts, reducing administrative overhead.
    
    Rationale: Storing log data on a remote host protects log integrity from local attacks. If an attacker gains root access on the local system, they could tamper with or remove log data that is stored on the local system
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "4.2.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"rsyslog.conf(5) - Linux man page", :url=>"https://linux.die.net/man/5/rsyslog.conf"})
  if RSYSLOG_HOST == ''
    describe parse_config_file('/etc/rsyslog.conf').params do
      it { should include /^(\s+)?\*\.\*\s+@@.*/ }
    end
  else
    describe parse_config_file('/etc/rsyslog.conf').params do
      it { should include ("*.* @@#{RSYSLOG_HOST}") }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.2.4_Ensure_syslog-ng_is_configured_to_send_logs_to_a_remote_log_host" do
  title "Ensure syslog-ng is configured to send logs to a remote log host"
  desc  "
    The syslog-ng utility supports the ability to send logs it gathers to a remote log host or to receive messages from remote hosts, reducing administrative overhead.
    
    Rationale: Storing log data on a remote host protects log integrity from local attacks. If an attacker gains root access on the local system, they could tamper with or remove log data that is stored on the local system
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "4.2.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"rsyslog.conf(5) - Linux man page", :url=>"https://linux.die.net/man/5/rsyslog.conf"})
  describe parse_config_file('/etc/syslog-ng/syslog-ng.conf').params do
    it { should include /^\s*destination logserver \{ tcp\(\"[\S|\.]+?\" port\([1-9]+\)\)\; \}\; log \{ source\(src\)\; destination\(logserver\)\; \}\;\s*$/ }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.4_Ensure_permissions_on_etccron.daily_are_configured" do
  title "Ensure permissions on /etc/cron.daily are configured"
  desc  "
    The /etc/cron.daily directory contains system cron jobs that need to run on a daily basis. The files in this directory cannot be manipulated by the crontab command, but are instead edited by system administrators using a text editor. The commands below restrict read/write and search access to user and group root, preventing regular users from accessing this directory.
    
    Rationale: Granting write access to this directory for non-privileged users could provide them the means for gaining unauthorized elevated privileges. Granting read access to this directory could give an unprivileged user insight in how to gain elevated privileges or circumvent auditing controls.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/cron.daily') do
    it { should exist }
    it { should be_directory }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.4_Ensure_SSH_X11_forwarding_is_disabled" do
  title "Ensure SSH X11 forwarding is disabled"
  desc  "
    The X11Forwarding parameter provides the ability to tunnel X11 traffic through the connection to enable remote graphic connections.
    
    Rationale: Disable X11 forwarding unless there is an operational requirement to use X11 applications directly. There is a small risk that the remote X11 servers of users who are logged in via SSH with X11 forwarding could be compromised by other users on the X11 server. Note that even if X11 forwarding is disabled, users can always install their own forwarders.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('X11Forwarding') { should eq 'no' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.3.4_Ensure_password_hashing_algorithm_is_SHA-512" do
  title "Ensure password hashing algorithm is SHA-512"
  desc  "
    The commands below change password encryption from md5 to sha512 (a much stronger hashing algorithm). All existing accounts will need to perform a password change to upgrade the stored hashes to the new algorithm.
    
    Rationale: The SHA-512 algorithm provides much stronger hashing than MD5, thus providing additional protection to the system by increasing the level of effort for an attacker to successfully determine passwords.
    
    Note that these change only apply to accounts configured on the local system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.3.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/pam.d/system-auth") do
    its("content") { should match(/^\s*password\s+sufficient\s+pam\_unix\.so\s+(\S+\s+)*sha512\s*(\S+\s+)*$/) }
  end
  describe file("/etc/pam.d/password-auth") do
    its("content") { should match(/^\s*password\s+sufficient\s+pam\_unix\.so\s+(\S+\s+)*sha512\s*(\S+\s+)*$/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.4.1.4_Ensure_inactive_password_lock_is_30_days_or_less" do
  title "Ensure inactive password lock is 30 days or less"
  desc  "
    User accounts that have been inactive for over a given period of time can be automatically disabled. It is recommended that accounts that are inactive for 30 days after password expiration be disabled.
    
    Rationale: Inactive accounts pose a threat to system security since the users are not logging in to notice failed login attempts or other anomalies.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.4.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe parse_config_file("/etc/default/useradd") do
    its('INACTIVE') { should cmp <=30 }
  end
  users = shadow.filter(password:/[^!|*]/).params. map { |x| x['user'] }
  unless users.empty?
    users.each do |user|
      describe shadow.users(user) do
        its('inactive_days') { should cmp <= 30 }
      end
    end
  else
    describe 'user password' do
      skip 'no user with password detected'
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.4_Ensure_permissions_on_etcgroup_are_configured" do
  title "Ensure permissions on /etc/group are configured"
  desc  "
    The /etc/group file contains a list of all the valid groups defined in the system. The command below allows read/write access for root and read access for everyone else.
    
    Rationale: The /etc/group file needs to be protected from unauthorized changes by non-privileged users, but needs to be readable as this information is used with many non-privileged programs.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/group") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.4_Ensure_no_legacy__entries_exist_in_etcgroup" do
  title "Ensure no legacy \"+\" entries exist in /etc/group"
  desc  "
    The character + in various files used to be markers for systems to insert data from NIS maps at a certain point in a system configuration file. These entries are no longer required on most systems, but may exist in files that have been imported from other platforms.
    
    Rationale: These entries may provide an avenue for attackers to gain privileged access on the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.4"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe etc_group do
    its('groups') { should_not include '+' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.5_Ensure_mounting_of_hfsplus_filesystems_is_disabled" do
  title "Ensure mounting of hfsplus filesystems is disabled"
  desc  "
    The hfsplus filesystem type is a hierarchical filesystem designed to replace hfs that allows you to mount Mac OS filesystems.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('hfsplus') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.1.5_Ensure_permissions_on_etcissue_are_configured" do
  title "Ensure permissions on /etc/issue are configured"
  desc  "
    The contents of the /etc/issue file are displayed to users prior to login for local terminals.
    
    Rationale: If the /etc/issue file does not have the correct ownership it could be modified by unauthorized users with incorrect or misleading information.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.7.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/issue") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.5_Ensure_time_services_are_not_enabled" do
  title "Ensure time services are not enabled"
  desc  "
    timeis a network service that responds with the server's current date and time as a 32 bit integer. This service is intended for debugging and testing purposes. It is recommended that this service be disabled.
    
    Rationale: Disabling this service will reduce the remote attack surface of the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  if package('xinetd').installed?
    describe xinetd_conf.services('time') do
      it { should be_disabled }
    end
  else
    describe package('xinetd') do
      it { should_not be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.6_Ensure_LDAP_server_is_not_enabled" do
  title "Ensure LDAP server is not enabled"
  desc  "
    The Lightweight Directory Access Protocol (LDAP) was introduced as a replacement for NIS/YP. It is a service that provides a method for looking up information from a central database.
    
    Rationale: If the system will not need to act as an LDAP server, it is recommended that the software be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('slapd') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.3.5_Ensure_LDAP_client_is_not_installed" do
  title "Ensure LDAP client is not installed"
  desc  "
    The Lightweight Directory Access Protocol (LDAP) was introduced as a replacement for NIS/YP. It is a service that provides a method for looking up information from a central database.
    
    Rationale: If the system will not need to act as an LDAP client, it is recommended that the software be removed to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.3.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe package('openldap-clients') do
    it { should_not be_installed }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.5_Ensure_broadcast_ICMP_requests_are_ignored" do
  title "Ensure broadcast ICMP requests are ignored"
  desc  "
    Setting net.ipv4.icmp_echo_ignore_broadcasts to 1 will cause the system to ignore all ICMP echo and timestamp requests to broadcast and multicast addresses.
    
    Rationale: Accepting ICMP echo and timestamp requests with broadcast or multicast destinations for your network could be used to trick your host into starting (or participating) in a Smurf attack. A Smurf attack relies on an attacker sending large amounts of ICMP broadcast messages with a spoofed source address. All hosts receiving this message and responding would send echo-reply messages back to the spoofed address, which is probably not routable. If many hosts respond to the packets, the amount of traffic on the network could be significantly multiplied.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.icmp_echo_ignore_broadcasts') do
    its('value') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.4.5_Ensure_permissions_on_etchosts.deny_are_644" do
  title "Ensure permissions on /etc/hosts.deny are 644"
  desc  "
    The /etc/hosts.deny file contains network information that is used by many system applications and therefore must be readable for these applications to operate.
    
    Rationale: It is critical to ensure that the /etc/hosts.deny file is protected from unauthorized write access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.4.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/hosts.deny') do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.6.5_Ensure_firewall_rules_exist_for_all_open_ports" do
  title "Ensure firewall rules exist for all open ports"
  desc  "
    Any ports that have been opened on non-loopback addresses need firewall rules to govern traffic.
    
    Rationale: Without a firewall rule configured for open ports default firewall policy will drop all packets to these ports.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.6.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  port.where { protocol =~ /.*/ && port >= 0 && address =~ /^(?!127\.0\.0\.1|::1|::).*$/ }.entries.each do |entry|
    rule_inbound = "-A INPUT -p #{entry[:protocol]} -m #{entry[:protocol]} --dport #{entry[:port]} -m state --state NEW,ESTABLISHED -j ACCEPT"
    rule_outbound = "-A OUTPUT -p #{entry[:protocol]} -m #{entry[:protocol]} --sport #{entry[:port]} -m state --state ESTABLISHED -j ACCEPT"
    describe iptables do
      it { should have_rule(rule_inbound) }
      it { should have_rule(rule_outbound) }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.1.5_Ensure_remote_rsyslog_messages_are_only_accepted_on_designated_log_hosts." do
  title "Ensure remote rsyslog messages are only accepted on designated log hosts."
  desc  "
    By default, rsyslog does not listen for log messages coming in from remote systems. The ModLoad tells rsyslog to load the imtcp.so module so it can listen over a network via TCP. The InputTCPServerRun option instructs rsyslogd to listen on the specified TCP port.
    
    Rationale: The guidance in the section ensures that remote log hosts are configured to only accept rsyslog data from hosts within the specified domain and that those systems that are not designed to be log hosts do not accept any remote rsyslog messages. This provides protection from spoofed log data and ensures that system administrators are reviewing reasonably complete syslog data in a central location.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "4.2.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"rsyslogd - Linux man page", :url=>"https://linux.die.net/man/8/rsyslogd"})
  if DESIGNATED_LOG_HOST == true
    describe parse_config_file('/etc/rsyslog.conf').params  do
      it { should include ('$ModLoad imtcp.so') }
      it { should include ('$InputTCPServerRun 514') }
    end
  else
    describe parse_config_file('/etc/rsyslog.conf').params  do
      it { should_not include ('$ModLoad imtcp.so') }
      it { should_not include ('$InputTCPServerRun 514') }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_4.2.2.5_Ensure_remote_syslog-ng_messages_are_only_accepted_on_designated_log_hosts" do
  title "Ensure remote syslog-ng messages are only accepted on designated log hosts"
  desc  "
    By default, syslog-ng does not listen for log messages coming in from remote systems.
    
    Rationale: The guidance in the section ensures that remote log hosts are configured to only accept syslog-ng data from hosts within the specified domain and that those systems that are not designed to be log hosts do not accept any remote syslog-ng messages. This provides protection from spoofed log data and ensures that system administrators are reviewing reasonably complete syslog data in a central location.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "4.2.2.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"rsyslog.conf(5) - Linux man page", :url=>"https://linux.die.net/man/5/rsyslog.conf"})
  describe 'remote syslog-ng' do
    skip 'Review the /etc/syslog-ng/syslog-ng.conf file and verify the following lines are configured appropriately on designated log hosts:
  
      source net{ tcp(); };
      destination remote { file("/var/log/remote/${FULLHOST}-log"); };
      log { source(net); destination(remote); };
  
    On non designated log hosts edit the /etc/syslog-ng/syslog-ng.conf file and remove or edit any sources that accept network sourced log messages.'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.5_Ensure_permissions_on_etccron.weekly_are_configured" do
  title "Ensure permissions on /etc/cron.weekly are configured"
  desc  "
    The /etc/cron.weekly directory contains system cron jobs that need to run on a weekly basis. The files in this directory cannot be manipulated by the crontab command, but are instead edited by system administrators using a text editor. The commands below restrict read/write and search access to user and group root, preventing regular users from accessing this directory.
    
    Rationale: Granting write access to this directory for non-privileged users could provide them the means for gaining unauthorized elevated privileges. Granting read access to this directory could give an unprivileged user insight in how to gain elevated privileges or circumvent auditing controls.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/cron.weekly') do
    it { should exist }
    it { should be_directory }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.5_Ensure_SSH_MaxAuthTries_is_set_to_4_or_less" do
  title "Ensure SSH MaxAuthTries is set to 4 or less"
  desc  "
    The MaxAuthTries parameter specifies the maximum number of authentication attempts permitted per connection. When the login failure count reaches half the number, error messages will be written to the syslog file detailing the login failure.
    
    Rationale: Setting the MaxAuthTries parameter to a low number will minimize the risk of successful brute force attacks to the SSH server. While the recommended setting is 4, set the number based on site policy.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('MaxAuthTries') { should cmp <= 4 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.5_Ensure_permissions_on_etcgshadow_are_configured" do
  title "Ensure permissions on /etc/gshadow are configured"
  desc  "
    The /etc/gshadow file is used to store the information about groups that is critical to the security of those accounts, such as the hashed password and other security information.
    
    Rationale: If attackers can gain read access to the /etc/gshadow file, they can easily run a password cracking program against the hashed password to break it. Other security information that is stored in the /etc/gshadow file (such as group administrators) could also be useful to subvert the group.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/gshadow") do
    it { should exist }
    it { should be_file }
    it { should_not be_readable }
    it { should_not be_writable }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.5_Ensure_root_is_the_only_UID_0_account" do
  title "Ensure root is the only UID 0 account"
  desc  "
    Any account with UID 0 has superuser privileges on the system.
    
    Rationale: This access must be limited to only the default root account and only from the system console. Administrative access must be through an unprivileged account using an approved mechanism as noted in Item 5.6 Ensure access to the su command is restricted.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.5"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe passwd.uids(0) do
    its('users') { should cmp 'root' }
    its('count') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.6_Ensure_mounting_of_squashfs_filesystems_is_disabled" do
  title "Ensure mounting of squashfs filesystems is disabled"
  desc  "
    The squashfs filesystem type is a compressed read-only Linux filesystem embedded in small footprint systems (similar to cramfs ). A squashfs image can be used without having to first decompress the image.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('squashfs') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.7.1.6_Ensure_permissions_on_etcissue.net_are_configured" do
  title "Ensure permissions on /etc/issue.net are configured"
  desc  "
    The contents of the /etc/issue.net file are displayed to users prior to login for remote connections from configured services.
    
    Rationale: If the /etc/issue.net file does not have the correct ownership it could be modified by unauthorized users with incorrect or misleading information.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.7.1.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/issue.net") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.6_Ensure_tftp_server_is_not_enabled" do
  title "Ensure tftp server is not enabled"
  desc  "
    Trivial File Transfer Protocol (TFTP) is a simple file transfer protocol, typically used to automatically transfer configuration or boot machines from a boot server. The packages tftp and atftp are both used to define and support a TFTP server.
    
    Rationale: TFTP does not support authentication nor does it ensure the confidentiality or integrity of data. It is recommended that TFTP be removed, unless there is a specific need for TFTP. In that case, extreme caution must be used when configuring the services.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  if package('xinetd').installed?
    describe xinetd_conf.services('tftp') do
      it { should be_disabled }
    end
  else
    describe package('xinetd') do
      it { should_not be_installed }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.7_Ensure_NFS_and_RPC_are_not_enabled" do
  title "Ensure NFS and RPC are not enabled"
  desc  "
    The Network File System (NFS) is one of the first and most widely distributed file systems in the UNIX environment. It provides the ability for systems to mount file systems of other servers through the network.
    
    Rationale: If the system does not export NFS shares or act as an NFS client, it is recommended that these services be disabled to reduce remote attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('nfs') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
  describe service("rpcbind") do
    it { should_not be_enabled }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.6_Ensure_bogus_ICMP_responses_are_ignored" do
  title "Ensure bogus ICMP responses are ignored"
  desc  "
    Setting icmp_ignore_bogus_error_responses to 1 prevents the kernel from logging bogus responses (RFC-1122 non-compliant) from broadcast reframes, keeping file systems from filling up with useless log messages.
    
    Rationale: Some routers (and some attackers) will send responses that violate RFC-1122 and attempt to fill up a log file system with many useless error messages.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.icmp_ignore_bogus_error_responses') do
    its('value') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.6_Ensure_permissions_on_etccron.monthly_are_configured" do
  title "Ensure permissions on /etc/cron.monthly are configured"
  desc  "
    The /etc/cron.monthly directory contains system cron jobs that need to run on a monthly basis. The files in this directory cannot be manipulated by the crontab command, but are instead edited by system administrators using a text editor. The commands below restrict read/write and search access to user and group root, preventing regular users from accessing this directory.
    
    Rationale: Granting write access to this directory for non-privileged users could provide them the means for gaining unauthorized elevated privileges. Granting read access to this directory could give an unprivileged user insight in how to gain elevated privileges or circumvent auditing controls.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/cron.monthly') do
    it { should exist }
    it { should be_directory }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.6_Ensure_SSH_IgnoreRhosts_is_enabled" do
  title "Ensure SSH IgnoreRhosts is enabled"
  desc  "
    The IgnoreRhosts parameter specifies that .rhosts and .shosts files will not be used in RhostsRSAAuthentication or HostbasedAuthentication .
    
    Rationale: Setting this parameter forces users to enter a password when authenticating with ssh.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('IgnoreRhosts') { should eq 'yes' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.6_Ensure_permissions_on_etcpasswd-_are_configured" do
  title "Ensure permissions on /etc/passwd- are configured"
  desc  "
    The /etc/passwd- file contains backup user account information.
    
    Rationale: It is critical to ensure that the /etc/passwd- file is protected from unauthorized access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/passwd-") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.6_Ensure_root_PATH_Integrity" do
  title "Ensure root PATH Integrity"
  desc  "
    The root user can execute any command on the system and could be fooled into executing programs unintentionally if the PATH is not set correctly.
    
    Rationale: Including the current working directory (.) or other writable directory in root 's executable path makes it likely that an attacker can gain superuser access by forcing an administrator operating as root to execute a Trojan horse program.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.6"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe os_env('PATH').content.to_s.split(':') do
    it { should_not be_empty }
  end
  os_env('PATH').content.to_s.split(':').each do |entry|
    describe entry do
      it { should_not eq "" }
      it { should_not eq "." }
    end
  end
  os_env('PATH').content.to_s.split(':').each do |entry|
    describe file(entry) do
      it { should exist }
      it { should_not be_writable.by 'group' }
      it { should_not be_writable.by 'other' }
      its( 'uid' ) { should cmp 0 }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.8_Ensure_nodev_option_set_on_vartmp_partition" do
  title "Ensure nodev option set on /var/tmp partition"
  desc  "
    The nodev mount option specifies that the filesystem cannot contain special devices.
    
    Rationale: Since the /var/tmp filesystem is not intended to support devices, set this option to ensure that users cannot attempt to create block or character special devices in /var/tmp .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/var/tmp') do
    its('options') { should include 'nodev' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.7_Ensure_mounting_of_udf_filesystems_is_disabled" do
  title "Ensure mounting of udf filesystems is disabled"
  desc  "
    The udf filesystem type is the universal disk format used to implement ISO/IEC 13346 and ECMA-167 specifications. This is an open vendor filesystem type for data storage on a broad range of media. This filesystem type is necessary to support writing DVDs and newer optical disc formats.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('udf') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.1.7_Ensure_xinetd_is_not_enabled" do
  title "Ensure xinetd is not enabled"
  desc  "
    The eXtended InterNET Daemon ( xinetd ) is an open source super daemon that replaced the original inetd daemon. The xinetd daemon listens for well known services and dispatches the appropriate daemon to properly respond to service requests.
    
    Rationale: If there are no xinetd services required, it is recommended that the daemon be disabled.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.1.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('xinetd') do
    it { should_not be_enabled }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.8_Ensure_DNS_Server_is_not_enabled" do
  title "Ensure DNS Server is not enabled"
  desc  "
    The Domain Name System (DNS) is a hierarchical naming system that maps names to IP addresses for computers, services and other resources connected to a network.
    
    Rationale: Unless a system is specifically designated to act as a DNS server, it is recommended that the service be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('named') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.7_Ensure_Reverse_Path_Filtering_is_enabled" do
  title "Ensure Reverse Path Filtering is enabled"
  desc  "
    Setting net.ipv4.conf.all.rp_filter and net.ipv4.conf.default.rp_filter to 1 forces the Linux kernel to utilize reverse path filtering on a received packet to determine if the packet was valid. Essentially, with reverse path filtering, if the return packet does not go out the same interface that the corresponding source packet came from, the packet is dropped (and logged if log_martians is set).
    
    Rationale: Setting these flags is a good way to deter attackers from sending your system bogus packets that cannot be responded to. One instance where this feature breaks down is if asymmetrical routing is employed. This would occur when using dynamic routing protocols (bgp, ospf, etc) on your system. If you are using asymmetrical routing on your system, you will not be able to enable this feature without breaking the routing.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.conf.all.rp_filter') do
    its('value') { should eq 1 }
  end
  describe kernel_parameter('net.ipv4.conf.default.rp_filter') do
    its('value') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.7_Ensure_permissions_on_etccron.d_are_configured" do
  title "Ensure permissions on /etc/cron.d are configured"
  desc  "
    The /etc/cron.d directory contains system cron jobs that need to run in a similar manner to the hourly, daily weekly and monthly jobs from /etc/crontab , but require more granular control as to when they run. The files in this directory cannot be manipulated by the crontab command, but are instead edited by system administrators using a text editor. The commands below restrict read/write and search access to user and group root, preventing regular users from accessing this directory.
    
    Rationale: Granting write access to this directory for non-privileged users could provide them the means for gaining unauthorized elevated privileges. Granting read access to this directory could give an unprivileged user insight in how to gain elevated privileges or circumvent auditing controls.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/cron.d') do
    it { should exist }
    it { should be_directory }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.7_Ensure_SSH_HostbasedAuthentication_is_disabled" do
  title "Ensure SSH HostbasedAuthentication is disabled"
  desc  "
    The HostbasedAuthentication parameter specifies if authentication is allowed through trusted hosts via the user of .rhosts , or /etc/hosts.equiv , along with successful public key client host authentication. This option only applies to SSH Protocol Version 2.
    
    Rationale: Even though the .rhosts files are ineffective if support is disabled in /etc/pam.conf , disabling the ability to use .rhosts files in SSH provides an additional layer of protection .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('HostbasedAuthentication') { should eq 'no' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.7_Ensure_permissions_on_etcshadow-_are_configured" do
  title "Ensure permissions on /etc/shadow- are configured"
  desc  "
    The /etc/shadow- file is used to store backup information about user accounts that is critical to the security of those accounts, such as the hashed password and other security information.
    
    Rationale: It is critical to ensure that the /etc/shadow- file is protected from unauthorized access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/shadow-") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.7_Ensure_all_users_home_directories_exist" do
  title "Ensure all users' home directories exist"
  desc  "
    Users can be defined in /etc/passwd without a home directory or with a home directory that does not actually exist.
    
    Rationale: If the user's home directory does not exist or is unassigned, the user will be placed in \"/\" and will not be able to write any files or have local environment variables set.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.7"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes.each do |homefolder|
    describe file(homefolder) do
      it {should exist }
      it {should be_directory }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.9_Ensure_nosuid_option_set_on_vartmp_partition" do
  title "Ensure nosuid option set on /var/tmp partition"
  desc  "
    The nosuid mount option specifies that the filesystem cannot contain setuid files.
    
    Rationale: Since the /var/tmp filesystem is only intended for temporary file storage, set this option to ensure that users cannot create setuid files in /var/tmp .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.9"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/var/tmp') do
    its('options') { should include 'nosuid' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.1.8_Ensure_mounting_of_FAT_filesystems_is_disabled" do
  title "Ensure mounting of FAT filesystems is disabled"
  desc  "
    The FAT filesystem format is primarily used on older windows systems and portable USB drives or flash modules. It comes in three types FAT12 , FAT16 , and FAT32 all of which are supported by the vfat kernel module.
    
    Rationale: Removing support for unneeded filesystem types reduces the local attack surface of the system. If this filesystem type is not needed, disable it.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.1.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_module('vfat') do
    it { should_not be_loaded }
    it { should be_disabled }
    it { should be_blacklisted }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.9_Ensure_FTP_Server_is_not_enabled" do
  title "Ensure FTP Server is not enabled"
  desc  "
    The File Transfer Protocol (FTP) provides networked computers with the ability to transfer files.
    
    Rationale: FTP does not protect the confidentiality of data or authentication credentials. It is recommended sftp be used if file transfer is required. Unless there is a need to run the system as a FTP server (for example, to allow anonymous downloads), it is recommended that the service be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.9"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('vsftpd') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_3.2.8_Ensure_TCP_SYN_Cookies_is_enabled" do
  title "Ensure TCP SYN Cookies is enabled"
  desc  "
    When tcp_syncookies is set, the kernel will handle TCP SYN packets normally until the half-open connection queue is full, at which time, the SYN cookie functionality kicks in. SYN cookies work by not using the SYN queue at all. Instead, the kernel simply replies to the SYN with a SYN|ACK, but will include a specially crafted TCP sequence number that encodes the source and destination IP address and port number and the time the packet was sent. A legitimate connection would send the ACK packet of the three way handshake with the specially crafted sequence number. This allows the system to verify that it has received a valid response to a SYN cookie and allow the connection, even though there is no corresponding SYN in the queue.
    
    Rationale: Attackers use SYN flood attacks to perform a denial of service attacked on a system by sending many SYN packets without completing the three way handshake. This will quickly use up slots in the kernel's half-open connection queue and prevent legitimate connections from succeeding. SYN cookies allow the system to keep accepting valid connections, even if under a denial of service attack.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "3.2.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe kernel_parameter('net.ipv4.tcp_syncookies') do
    its('value') { should eq 1 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.1.8_Ensure_atcron_is_restricted_to_authorized_users" do
  title "Ensure at/cron is restricted to authorized users"
  desc  "
    Configure /etc/cron.allow and /etc/at.allow to allow specific users to use these services. If /etc/cron.allow or /etc/at.allow do not exist, then /etc/at.deny and /etc/cron.deny are checked. Any user not specifically defined in those files is allowed to use at and cron. By removing the files, only users in /etc/cron.allow and /etc/at.allow are allowed to use at and cron. Note that even though a given user is not listed in cron.allow , cron jobs can still be run as that user. The cron.allow file only controls administrative access to the crontab command for scheduling and modifying cron jobs.
    
    Rationale: On many systems, only the system administrator is authorized to schedule cron jobs. Using the cron.allow file to control who can run cron jobs enforces this policy. It is easier to manage an allow list than a deny list. In a deny list, you could potentially add a user ID to the system and forget to add it to the deny files.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.1.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file('/etc/cron.deny') do
    it { should_not exist }
  end
  describe file('/etc/at.deny') do
    it { should_not exist }
  end
  describe file('/etc/cron.allow') do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
  describe file('/etc/at.allow') do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.8_Ensure_SSH_root_login_is_disabled" do
  title "Ensure SSH root login is disabled"
  desc  "
    The PermitRootLogin parameter specifies if the root user can log in using ssh(1). The default is no.
    
    Rationale: Disallowing root logins over SSH requires system admins to authenticate using their own individual account, then escalating to root via sudo or su . This in turn limits opportunity for non-repudiation and provides a clear audit trail in the event of a security incident
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('PermitRootLogin') { should eq 'no' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.8_Ensure_permissions_on_etcgroup-_are_configured" do
  title "Ensure permissions on /etc/group- are configured"
  desc  "
    The /etc/group- file contains a backup list of all the valid groups defined in the system.
    
    Rationale: It is critical to ensure that the /etc/group- file is protected from unauthorized access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/group-") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.8_Ensure_users_home_directories_permissions_are_750_or_more_restrictive" do
  title "Ensure users' home directories permissions are 750 or more restrictive"
  desc  "
    While the system administrator can establish secure permissions for users' home directories, the users can easily override these.
    
    Rationale: Group or world-writable user home directories may enable malicious users to steal or modify other users' data or to gain another user's system privileges.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.8"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes.each do |homefolder|
    describe file(homefolder) do
      it { should exist }
      it { should be_directory }
      it { should be_executable.by('owner') }
      it { should_not be_executable.by('other') }
      it { should be_writable.by('owner') }
      it { should_not be_writable.by('group') }
      it { should_not be_writable.by('other') }
      it { should be_readable.by('owner') }
      it { should_not be_readable.by('other') }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.10_Ensure_noexec_option_set_on_vartmp_partition" do
  title "Ensure noexec option set on /var/tmp partition"
  desc  "
    The noexec mount option specifies that the filesystem cannot contain executable binaries.
    
    Rationale: Since the /var/tmp filesystem is only intended for temporary file storage, set this option to ensure that users cannot run executable binaries from /var/tmp .
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.10"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/var/tmp') do
    its('options') { should include 'noexec' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.10_Ensure_HTTP_server_is_not_enabled" do
  title "Ensure HTTP server is not enabled"
  desc  "
    HTTP or web servers provide the ability to host web site content.
    
    Rationale: Unless there is a need to run the system as a web server, it is recommended that the service be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.10"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('httpd') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.9_Ensure_SSH_PermitEmptyPasswords_is_disabled" do
  title "Ensure SSH PermitEmptyPasswords is disabled"
  desc  "
    The PermitEmptyPasswords parameter specifies if the SSH server allows login to accounts with empty password strings.
    
    Rationale: Disallowing remote shell access to accounts that have an empty password reduces the probability of unauthorized access to the system
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.9"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('PermitEmptyPasswords') { should eq 'no' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.9_Ensure_permissions_on_etcgshadow-_are_configured" do
  title "Ensure permissions on /etc/gshadow- are configured"
  desc  "
    The /etc/gshadow- file is used to store backup information about groups that is critical to the security of those accounts, such as the hashed password and other security information.
    
    Rationale: It is critical to ensure that the /etc/gshadow- file is protected from unauthorized access. Although it is protected by default, the file permissions could be changed either inadvertently or through malicious actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.9"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe file("/etc/gshadow-") do
    it { should exist }
    it { should be_file }
    it { should be_owned_by 'root' }
    it { should be_grouped_into 'root' }
    it { should be_writable.by('owner') }
    it { should be_readable.by('owner') }
    it { should_not be_writable.by('group') }
    it { should_not be_readable.by('group') }
    it { should_not be_writable.by('other') }
    it { should_not be_readable.by('other') }
    it { should_not be_executable }
    its("sgid") { should equal false }
    its("sticky") { should equal false }
    its("suid") { should equal false }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.9_Ensure_users_own_their_home_directories" do
  title "Ensure users own their home directories"
  desc  "
    The user home directory is space defined for the particular user to set local environment variables and to store personal files.
    
    Rationale: Since the user is accountable for files stored in the user home directory, the user must be the owner of the directory.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.9"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  homeslist = {}
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.users.each_with_index { |k,i| homeslist[k] = passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes[i] }
  homeslist.each do |user,homefolder|
    describe file(homefolder) do
      it { should exist }
      it { should be_directory }
      it { should be_owned_by user }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.11_Ensure_IMAP_and_POP3_server_is_not_enabled" do
  title "Ensure IMAP and POP3 server is not enabled"
  desc  "
    dovecot is an open source IMAP and POP3 server for Linux based systems.
    
    Rationale: Unless POP3 and/or IMAP servers are to be provided by this system, it is recommended that the service be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.11"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('dovecot') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.10_Ensure_SSH_PermitUserEnvironment_is_disabled" do
  title "Ensure SSH PermitUserEnvironment is disabled"
  desc  "
    The PermitUserEnvironment option allows users to present environment options to the ssh daemon.
    
    Rationale: Permitting users the ability to set environment variables through the SSH daemon could potentially allow users to bypass security controls (e.g. setting an execution path that has ssh executing trojan'd programs)
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.10"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('PermitUserEnvironment') { should eq 'no' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.10_Ensure_no_world_writable_files_exist" do
  title "Ensure no world writable files exist"
  desc  "
    Unix-based systems support variable settings to control access to files. World writable files are the least secure. See the chmod(2) man page for more information.
    
    Rationale: Data in world-writable files can be modified and compromised by any user on the system. World writable files may also indicate an incorrectly written script or program that could potentially be the cause of a larger compromise to the system's integrity.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.10"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  world_writeable_files = command('df --local -P | awk {\'if (NR!=1) print $6\'} | xargs -I \'{}\' find \'{}\' -perm -0002 -type f ! -path \'/proc/*\' ! -path \'/sys/*\' -print 2>/dev/null').stdout.split(/\r?\n/)
  describe world_writeable_files do
    it { should eq [] }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.10_Ensure_users_dot_files_are_not_group_or_world_writable" do
  title "Ensure users' dot files are not group or world writable"
  desc  "
    While the system administrator can establish secure permissions for users' \"dot\" files, the users can easily override these.
    
    Rationale: Group or world-writable user configuration files may enable malicious users to steal or modify other users' data or to gain another user's system privileges.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.10"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  homeslist = {}
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.users.each_with_index {|k,i| homeslist[k] = passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes[i] }
  homeslist.each do |user,homefolder|
    dotfiles = command('ls -d ' + homefolder + '/.[A-Za-z0-9]*').stdout.split
    if !dotfiles.empty?
      dotfiles.each do |dotfile|
        if file(dotfile).exist?
          describe file(dotfile) do
            it { should exist }
            it { should be_owned_by user }
            it { should be_writable.by('owner') }
            it { should_not be_writable.by('group') }
            it { should_not be_writable.by('other') }
          end
        end
      end
    else
      describe 'Check write permissions for users dot files' do
        skip 'Ensure users dot files are not group or world writable.'
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.12_Ensure_Samba_is_not_enabled" do
  title "Ensure Samba is not enabled"
  desc  "
    The Samba daemon allows system administrators to configure their Linux systems to share file systems and directories with Windows desktops. Samba will advertise the file systems and directories via the Small Message Block (SMB) protocol. Windows desktop users will be able to mount these directories and file systems as letter drives on their systems.
    
    Rationale: If there is no need to mount directories and file systems to Windows systems, then this service can be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.12"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('smb') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.11_Ensure_only_approved_ciphers_are_used" do
  title "Ensure only approved ciphers are used"
  desc  "
    This variable limits the types of ciphers that SSH can use during communication.
    
    Rationale: Based on research conducted at various institutions, it was determined that the symmetric portion of the SSH Transport Protocol (as described in RFC 4253) has security weaknesses that allowed recovery of up to 32 bits of plaintext from a block of ciphertext that was encrypted with the Cipher Block Chaining (CBD) method. From that research, new Counter mode algorithms (as described in RFC4344) were designed that are not vulnerable to these types of attacks and these algorithms are now recommended for standard use.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.11"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"For more information on the Counter mode algorithms, read RFC4344", :url=>"http://www.ietf.org/rfc/rfc4344.txt"})
  describe sshd_config do
    its('Ciphers') { should_not match(/-cbc(,|$)/) }
  end
  describe sshd_config do
    its('Ciphers') { should match(/.+/) }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.11_Ensure_no_unowned_files_or_directories_exist" do
  title "Ensure no unowned files or directories exist"
  desc  "
    Sometimes when administrators delete users from the password file they neglect to remove all files owned by those users from the system.
    
    Rationale: A new user who is assigned the deleted user's user ID or group ID may then end up \"owning\" these files, and thus have more access on the system than was intended.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.11"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  file_no_user = command('df --local -P | awk {\'if (NR!=1) print $6\'} | xargs -I \'{}\' find \'{}\' -xdev -nouser').stdout.split(/\r?\n/)
  describe file_no_user do
    it { should eq [] }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.11_Ensure_no_users_have_.forward_files" do
  title "Ensure no users have .forward files"
  desc  "
    The .forward file specifies an email address to forward the user's mail to.
    
    Rationale: Use of the .forward file poses a security risk in that sensitive data may be inadvertently transferred outside the organization. The .forward file also poses a risk as it can be used to execute commands that may perform unintended actions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.11"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  homeslist = {}
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.users.each_with_index {|k,i| homeslist[k] = passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes[i] }
  homeslist.each do |user,homefolder|
    describe file(homefolder + '/.forward') do
      it { should_not exist }
    end
    hostbasedforwardfile = command('ls ' + homefolder + '/.forward.*').stdout.split
    if ! hostbasedforwardfile.empty?
      hostbasedforwardfile.each do |forwardfile|
        describe file(forwardfile) do
          it { should_not exist }
        end
      end
    else
      describe file(homefolder + '/.forward.example.com') do
        it { should_not exist }
      end
    end
    describe file(homefolder + '/.forward+') do
      it { should_not exist }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.13_Ensure_HTTP_Proxy_Server_is_not_enabled" do
  title "Ensure HTTP Proxy Server is not enabled"
  desc  "
    Squid is a standard proxy server used in many distributions and environments.
    
    Rationale: If there is no need for a proxy server, it is recommended that the squid proxy be disabled to reduce the potential attack surface.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.13"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('squid') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.12_Ensure_only_approved_MAC_algorithms_are_used" do
  title "Ensure only approved MAC algorithms are used"
  desc  "
    This variable limits the types of MAC algorithms that SSH can use during communication.
    
    Rationale: MD5 and 96-bit MAC algorithms are considered weak and have been shown to increase exploitability in SSH downgrade attacks. Weak algorithms continue to have a great deal of attention as a weak spot that can be exploited with expanded computing power. An attacker that breaks the algorithm could take advantage of a MiTM position to decrypt the SSH tunnel and capture credentials and information
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.12"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  ref   ({:ref=>"More information on SSH downgrade attacks can be found here", :url=>"http://www.mitls.org/pages/attacks/SLOTH"})
  describe sshd_config do
    its('MACs') { should eq MACS }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.12_Ensure_no_ungrouped_files_or_directories_exist" do
  title "Ensure no ungrouped files or directories exist"
  desc  "
    Sometimes when administrators delete users or groups from the system they neglect to remove all files owned by those users or groups.
    
    Rationale: A new user who is assigned the deleted user's user ID or group ID may then end up \"owning\" these files, and thus have more access on the system than was intended.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.1.12"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  file_no_group = command('df --local -P | awk {\'if (NR!=1) print $6\'} | xargs -I \'{}\' find \'{}\' -xdev -nogroup').stdout.split(/\r?\n/)
  describe file_no_group do
    it { should eq [] }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.12_Ensure_no_users_have_.netrc_files" do
  title "Ensure no users have .netrc files"
  desc  "
    The .netrc file contains data for logging into a remote host for file transfers via FTP.
    
    Rationale: The .netrc file presents a significant security risk since it stores passwords in unencrypted form. Even if FTP is disabled, user accounts may have brought over .netrc files from other systems which could pose a risk to those systems.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.12"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  homeslist = {}
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.users.each_with_index {|k,i| homeslist[k] = passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes[i] }
  homeslist.each do |user,homefolder|
    describe file(homefolder + '/.netrc') do
      it { should_not exist }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.14_Ensure_nodev_option_set_on_home_partition" do
  title "Ensure nodev option set on /home partition"
  desc  "
    The nodev mount option specifies that the filesystem cannot contain special devices.
    
    Rationale: Since the user partitions are not intended to support devices, set this option to ensure that users cannot attempt to create block or character special devices.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.14"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/home') do
    its('options') { should include 'nodev' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.14_Ensure_SNMP_Server_is_not_enabled" do
  title "Ensure SNMP Server is not enabled"
  desc  "
    The Simple Network Management Protocol (SNMP) server is used to listen for SNMP commands from an SNMP management system, execute the commands or collect the information and then send results back to the requesting system.
    
    Rationale: The SNMP server communicates using SNMP v1, which transmits data in the clear and does not require authentication to execute commands. Unless absolutely necessary, it is recommended that the SNMP service not be used.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.14"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('snmpd') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.13_Ensure_SSH_Idle_Timeout_Interval_is_configured" do
  title "Ensure SSH Idle Timeout Interval is configured"
  desc  "
    The two options ClientAliveInterval and ClientAliveCountMax control the timeout of ssh sessions. When the ClientAliveInterval variable is set, ssh sessions that have no activity for the specified length of time are terminated. When the ClientAliveCountMax variable is set, sshd will send client alive messages at every ClientAliveInterval interval. When the number of consecutive client alive messages are sent with no response from the client, the ssh session is terminated. For example, if the ClientAliveInterval is set to 15 seconds and the ClientAliveCountMax is set to 3, the client ssh session will be terminated after 45 seconds of idle time.
    
    Rationale: Having no timeout value associated with a connection could allow an unauthorized user access to another user's ssh session (e.g. user walks away from their computer and doesn't lock the screen). Setting a timeout value at least reduces the risk of this happening..
    
    While the recommended setting is 300 seconds (5 minutes), set this timeout value based on site policy. The recommended setting for ClientAliveCountMax is 0. In this case, the client session will be terminated after 5 minutes of idle time and no keepalive messages will be sent.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.13"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('ClientAliveInterval') { should cmp 300 }
    its('ClientAliveCountMax') { should cmp 0 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.13_Audit_SUID_executables" do
  title "Audit SUID executables"
  desc  "
    The owner of a file can set the file's permissions to run with the owner's or group's permissions, even if the user running the program is not the owner or a member of the group. The most common reason for a SUID program is to enable users to perform functions (such as changing their password) that require root privileges.
    
    Rationale: There are valid reasons for SUID programs, but it is important to identify and review such programs to ensure they are legitimate.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "6.1.13"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  find_suid_files = command('df --local -P | awk {\'if (NR!=1) print $6\'} | xargs -I \'{}\' find \'{}\' -xdev -type f -perm -4000').stdout.split(/\r?\n/)
  diff = find_suid_files & BLACKLIST
    describe diff do
      it { should be_empty }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.13_Ensure_users_.netrc_Files_are_not_group_or_world_accessible" do
  title "Ensure users' .netrc Files are not group or world accessible"
  desc  "
    While the system administrator can establish secure permissions for users' .netrc files, the users can easily override these.
    
    Rationale: .netrcfiles may contain unencrypted passwords that may be used to attack other systems.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.13"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  homeslist = {}
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.users.each_with_index {|k,i| homeslist[k] = passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes[i] }
  homeslist.each do |user,homefolder|
    if file(homefolder + '/.netrc').exist?
      describe file(homefolder + '/.netrc') do
          it { should be_owned_by user }
          it { should be_writable.by('owner') }
          it { should_not be_writable.by('group') }
          it { should_not be_writable.by('other') }
          it { should be_readable.by('owner') }
          it { should_not be_readable.by('group') }
          it { should_not be_readable.by('other') }
          it { should be_executable.by('owner') }
          it { should_not be_executable.by('group') }
          it { should_not be_executable.by('other') }
      end
    else
      describe 'Ensure users .netrc Files are not group or world accessible' do
        skip 'There is no .netrc file inside ' + user + '´s home directory.'
      end
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.15_Ensure_nodev_option_set_on_devshm_partition" do
  title "Ensure nodev option set on /dev/shm partition"
  desc  "
    The nodev mount option specifies that the filesystem cannot contain special devices.
    
    Rationale: Since the /dev/shm filesystem is not intended to support devices, set this option to ensure that users cannot attempt to create special devices in /dev/shm partitions.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.15"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/dev/shm') do
    its('options') { should include 'nodev' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.15_Ensure_mail_transfer_agent_is_configured_for_local-only_mode" do
  title "Ensure mail transfer agent is configured for local-only mode"
  desc  "
    Mail Transfer Agents (MTA), such as sendmail and Postfix, are used to listen for incoming mail and transfer the messages to the appropriate user or mail server. If the system is not intended to be a mail server, it is recommended that the MTA be configured to only process local mail.
    
    Rationale: The software for all Mail Transfer Agents is complex and most have a long history of security issues. While it is important to ensure that the system can process local mail messages, it is not necessary to have the MTA's daemon listening on a port unless the server is intended to be a mail server that receives and processes mail from other systems.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.15"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe port(25).where { protocol =~ /.*/ && address =~ /^(?!127\.0\.0\.1|::1).*$/ } do
    its('entries') { should be_empty }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.14_Ensure_SSH_LoginGraceTime_is_set_to_one_minute_or_less" do
  title "Ensure SSH LoginGraceTime is set to one minute or less"
  desc  "
    The LoginGraceTime parameter specifies the time allowed for successful authentication to the SSH server. The longer the Grace period is the more open unauthenticated connections can exist. Like other session controls in this session the Grace Period should be limited to appropriate organizational limits to ensure the service is available for needed access.
    
    Rationale: Setting the LoginGraceTime parameter to a low number will minimize the risk of successful brute force attacks to the SSH server. It will also limit the number of concurrent unauthenticated connections While the recommended setting is 60 seconds (1 Minute), set the number based on site policy.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.14"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('LoginGraceTime') { should cmp <= 60 }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.1.14_Audit_SGID_executables" do
  title "Audit SGID executables"
  desc  "
    The owner of a file can set the file's permissions to run with the owner's or group's permissions, even if the user running the program is not the owner or a member of the group. The most common reason for a SGID program is to enable users to perform functions (such as changing their password) that require root privileges.
    
    Rationale: There are valid reasons for SGID programs, but it is important to identify and review such programs to ensure they are legitimate. Review the files returned by the action in the audit section and check to see if system binaries have a different md5 checksum than what from the package. This is an indication that the binary may have been replaced.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "6.1.14"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  find_sgid_files = command('df --local -P | awk {\'if (NR!=1) print $6\'} | xargs -I \'{}\' find \'{}\' -xdev -type f -perm -2000').stdout.split(/\r?\n/)
  diff = find_sgid_files & BLACKLIST
    describe diff do
      it { should be_empty }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.14_Ensure_no_users_have_.rhosts_files" do
  title "Ensure no users have .rhosts files"
  desc  "
    While no .rhosts files are shipped by default, users can easily create them.
    
    Rationale: This action is only meaningful if .rhosts support is permitted in the file /etc/pam.conf . Even though the .rhosts files are ineffective if support is disabled in /etc/pam.conf , they may have been brought over from other systems and could contain information useful to an attacker for those other systems.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.14"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  homeslist = {}
  passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.users.each_with_index {|k,i| homeslist[k] = passwd.where{ shell != '/sbin/nologin' }.where{ shell != '/usr/bin/nologin' }.where{ shell != '/bin/nologin' }.where{ shell != '/bin/false' }.homes[i] }
  homeslist.each do |user,homefolder|
    describe file(homefolder + '/.rhosts') do
      it { should_not exist }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.16_Ensure_nosuid_option_set_on_devshm_partition" do
  title "Ensure nosuid option set on /dev/shm partition"
  desc  "
    The nosuid mount option specifies that the filesystem cannot contain setuid files.
    
    Rationale: Setting this option on a file system prevents users from introducing privileged programs onto the system and allowing non-root users to execute them.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.16"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/dev/shm') do
    its('options') { should include 'nosuid' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.16_Ensure_NIS_Server_is_not_enabled" do
  title "Ensure NIS Server is not enabled"
  desc  "
    The Network Information Service (NIS) (formally known as Yellow Pages) is a client-server directory service protocol for distributing system configuration files. The NIS server is a collection of programs that allow for the distribution of configuration files.
    
    Rationale: The NIS service is inherently an insecure system that has been vulnerable to DOS attacks, buffer overflows and has poor authentication for querying NIS maps. NIS generally been replaced by such protocols as Lightweight Directory Access Protocol (LDAP). It is recommended that the service be disabled and other, more secure services be used
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.16"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('ypserv') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.15_Ensure_SSH_access_is_limited" do
  title "Ensure SSH access is limited"
  desc  "
    There are several options available to limit which users and group can access the system via SSH. It is recommended that at least one of the following options be leveraged: AllowUsers
    
    The AllowUsers variable gives the system administrator the option of allowing specific users to ssh into the system. The list consists of space separated user names. Numeric user IDs are not recognized with this variable. If a system administrator wants to restrict user access further by only allowing the allowed users to log in from a particular host, the entry can be specified in the form of user@host. AllowGroups
    
    The AllowGroups variable gives the system administrator the option of allowing specific groups of users to ssh into the system. The list consists of space separated group names. Numeric group IDs are not recognized with this variable. DenyUsers
    
    The DenyUsers variable gives the system administrator the option of denying specific users to ssh into the system. The list consists of space separated user names. Numeric user IDs are not recognized with this variable. If a system administrator wants to restrict user access further by specifically denying a user's access from a particular host, the entry can be specified in the form of user@host. DenyGroups
    
    The DenyGroups variable gives the system administrator the option of denying specific groups of users to ssh into the system. The list consists of space separated group names. Numeric group IDs are not recognized with this variable.
    
    Rationale: Restricting which users can remotely access the system via SSH will help ensure that only authorized users access the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.15"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe.one do
    describe sshd_config do
      its('AllowUsers') { should match /[\S|\s]+/ }
    end
    describe sshd_config do
      its('AllowGroups') { should match /[\S|\s]+/ }
    end
    describe sshd_config do
      its('DenyUsers') { should match /[\S|\s]+/ }
    end
    describe sshd_config do
      its('DenyGroups') { should match /[\S|\s]+/ }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.15_Ensure_all_groups_in_etcpasswd_exist_in_etcgroup" do
  title "Ensure all groups in /etc/passwd exist in /etc/group"
  desc  "
    Over time, system administration errors and changes can lead to groups being defined in /etc/passwd but not in /etc/group .
    
    Rationale: Groups defined in the /etc/passwd file but not in the /etc/group file pose a threat to system security since group permissions are not properly managed.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.15"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  groupslist = {}
  passwd.uids.each_with_index {|k,i| groupslist[k] = passwd.gids[i] }
  groupslist.each do |uid,gid|
    describe etc_group() do
      its('gids') { should include gid }
    end
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.17_Ensure_noexec_option_set_on_devshm_partition" do
  title "Ensure noexec option set on /dev/shm partition"
  desc  "
    The noexec mount option specifies that the filesystem cannot contain executable binaries.
    
    Rationale: Setting this option on a file system prevents users from executing programs from shared memory. This deters users from introducing potentially malicious software on the system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.17"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe mount('/dev/shm') do
    its('options') { should include 'noexec' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.17_Ensure_rsh_server_is_not_enabled" do
  title "Ensure rsh server is not enabled"
  desc  "
    The Berkeley rsh-server ( rsh , rlogin , rexec ) package contains legacy services that exchange credentials in clear-text.
    
    Rationale: These legacy services contain numerous security exposures and have been replaced with the more secure SSH package.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.17"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('rsh.socket') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
  describe service('rlogin.socket') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
  describe service('rexec.socket') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_5.2.16_Ensure_SSH_warning_banner_is_configured" do
  title "Ensure SSH warning banner is configured"
  desc  "
    The Banner parameter specifies a file whose contents must be sent to the remote user before authentication is permitted. By default, no banner is displayed.
    
    Rationale: Banners are used to warn connecting users of the particular site's policy regarding connection. Presenting a warning message prior to the normal user login may assist the prosecution of trespassers on the computer system.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "5.2.16"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe sshd_config do
    its('Banner') { should eq '/etc/issue.net' }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.16_Ensure_no_duplicate_UIDs_exist" do
  title "Ensure no duplicate UIDs exist"
  desc  "
    Although the useradd program will not let you create a duplicate User ID (UID), it is possible for an administrator to manually edit the /etc/passwd file and change the UID field.
    
    Rationale: Users must be assigned unique UIDs for accountability and to ensure appropriate access protections.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.16"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe passwd() do
    its('uids') { should_not contain_duplicates }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.18_Ensure_nodev_option_set_on_removable_media_partitions" do
  title "Ensure nodev option set on removable media partitions"
  desc  "
    The nodev mount option specifies that the filesystem cannot contain special devices.
    
    Rationale: Removable media containing character and block special devices could be used to circumvent security controls by allowing non-root users to access sensitive device files such as /dev/kmem or the raw disk partitions.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.1.18"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'removable media partitions' do
    skip 'Ensure nodev option set on removable media partitions'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.18_Ensure_talk_server_is_not_enabled" do
  title "Ensure talk server is not enabled"
  desc  "
    The talk software makes it possible for users to send and receive messages across systems through a terminal session. The talk client (allows initiate of talk sessions) is installed by default.
    
    Rationale: The software presents a security risk as it uses unencrypted protocols for communication.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.18"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('ntalk') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.17_Ensure_no_duplicate_GIDs_exist" do
  title "Ensure no duplicate GIDs exist"
  desc  "
    Although the groupadd program will not let you create a duplicate Group ID (GID), it is possible for an administrator to manually edit the /etc/group file and change the GID field.
    
    Rationale: User groups must be assigned unique GIDs for accountability and to ensure appropriate access protections.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.17"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe etc_group() do
    its('gids') { should_not contain_duplicates }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.19_Ensure_nosuid_option_set_on_removable_media_partitions" do
  title "Ensure nosuid option set on removable media partitions"
  desc  "
    The nosuid mount option specifies that the filesystem cannot contain setuid files.
    
    Rationale: Setting this option on a file system prevents users from introducing privileged programs onto the system and allowing non-root users to execute them.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.1.19"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'removable media partitions' do
    skip 'Ensure nosuid option set on removable media partitions'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.19_Ensure_telnet_server_is_not_enabled" do
  title "Ensure telnet server is not enabled"
  desc  "
    The telnet-server package contains the telnet daemon, which accepts connections from users from other systems via the telnet protocol.
    
    Rationale: The telnet protocol is insecure and unencrypted. The use of an unencrypted transmission medium could allow a user with access to sniff network traffic the ability to steal credentials. The ssh package provides an encrypted session and stronger security.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.18"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('telnet.socket') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.18_Ensure_no_duplicate_user_names_exist" do
  title "Ensure no duplicate user names exist"
  desc  "
    Although the useradd program will not let you create a duplicate user name, it is possible for an administrator to manually edit the /etc/passwd file and change the user name.
    
    Rationale: If a user is assigned a duplicate user name, it will create and have access to files with the first UID for that username in /etc/passwd . For example, if \"test4\" has a UID of 1000 and a subsequent \"test4\" entry has a UID of 2000, logging in as \"test4\" will use UID 1000. Effectively, the UID is shared, which is a security problem.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.18"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe passwd() do
    its('users') { should_not contain_duplicates }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.20_Ensure_noexec_option_set_on_removable_media_partitions" do
  title "Ensure noexec option set on removable media partitions"
  desc  "
    The noexec mount option specifies that the filesystem cannot contain executable binaries.
    
    Rationale: Setting this option on a file system prevents users from executing programs from the removable media. This deters users from being able to introduce potentially malicious software on the system.
  "
  impact 0.0
  tag "cis-rhel7-2.1.1": "1.1.20"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe 'removable media partitions' do
    skip 'Ensure noexec option set on removable media partitions'
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.20_Ensure_tftp_server_is_not_enabled" do
  title "Ensure tftp server is not enabled"
  desc  "
    Trivial File Transfer Protocol (TFTP) is a simple file transfer protocol, typically used to automatically transfer configuration or boot machines from a boot server. The packages tftp and atftp are both used to define and support a TFTP server.
    
    Rationale: TFTP does not support authentication nor does it ensure the confidentiality or integrity of data. It is recommended that TFTP be removed, unless there is a specific need for TFTP. In that case, extreme caution must be used when configuring the services.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.20"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('tftp.socket') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_6.2.19_Ensure_no_duplicate_group_names_exist" do
  title "Ensure no duplicate group names exist"
  desc  "
    Although the groupadd program will not let you create a duplicate group name, it is possible for an administrator to manually edit the /etc/group file and change the group name.
    
    Rationale: If a group is assigned a duplicate group name, it will create and have access to files with the first GID for that group in /etc/group . Effectively, the GID is shared, which is a security problem.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "6.2.19"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe etc_group() do
    its('groups') { should_not contain_duplicates }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.21_Ensure_sticky_bit_is_set_on_all_world-writable_directories" do
  title "Ensure sticky bit is set on all world-writable directories"
  desc  "
    Setting the sticky bit on world writable directories prevents users from deleting or renaming files in that directory that are not owned by them.
    
    Rationale: This feature prevents the ability to delete or rename files in world writable directories (such as /tmp ) that are owned by another user.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.21"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe command('find / -path /proc -prune -o -type d \( -perm -0002 -a ! -perm -1000 \) -print 2>/dev/null') do
    its('stdout') { should eq ""}
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_2.2.21_Ensure_rsync_service_is_not_enabled" do
  title "Ensure rsync service is not enabled"
  desc  "
    The rsyncd service can be used to synchronize files between systems over network links.
    
    Rationale: The rsyncd service presents a security risk as it uses unencrypted protocols for communication.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "2.2.21"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service('rsyncd') do
    it { should_not be_enabled }
    it { should_not be_running }
  end
end

control "xccdf_org.cisecurity.benchmarks_rule_1.1.22_Disable_Automounting" do
  title "Disable Automounting"
  desc  "
    autofs allows automatic mounting of devices, typically including CD/DVDs and USB drives.
    
    Rationale: With automounting enabled anyone with physical access could attach a USB drive or disc and have its contents available in system even if they lacked permissions to mount it themselves.
  "
  impact 1.0
  tag "cis-rhel7-2.1.1": "1.1.22"
  tag "level": "1"
  tag "type": ["Server", "Workstation"]
  describe service("autofs") do
    it { should_not be_installed }
    it { should_not be_running }
    it { should_not be_enabled }
  end
end