#!/bin/bash
#  note: keep this script idempotent so it can run multiple times without issue

umask 0022

# lock it down
rm -f /etc/sudoers.d/90-cloud-init-users
rm -f /etc/sudoers.d/vagrant
usermod -G "" ${ssh_user}

# change the password for the ssh user
echo '${ssh_user}:${ssh_user_sudo_password}' | chpasswd

# Mount ${tmp_path} hardened
if grep /tmp /proc/mounts >/dev/null 2>&1; then
  mount -o remount,nodev,nosuid,noexec -t /tmp
fi

# whitelist our commands
cat << EOF > /etc/sudoers.d/automate
${ssh_user} ALL=(ALL) ${tmp_path}/install_hab.sh
${ssh_user} ALL=(ALL) ${tmp_path}/pg_provision.sh
${ssh_user} ALL=(ALL) ${tmp_path}/kibana_provision.sh
${ssh_user} ALL=(ALL) ${tmp_path}/metricbeat_provision.sh
${ssh_user} ALL=(ALL) ${tmp_path}/journalbeat_provision.sh
${ssh_user} ALL=(ALL) ${tmp_path}/tunables.sh
${ssh_user} ALL=(ALL) ${tmp_path}/automate_provision.sh
${ssh_user} ALL=(ALL) ${tmp_path}/es_provision.sh
${ssh_user} ALL=(ALL) /bin/su - root
# train runs many system commands to configure itself correctly
${ssh_user} ALL=(ALL) ALL
EOF
chmod 440 /etc/sudoers.d/automate

# set crazy umask
sed -i 's/umask 0.2/umask 0077/g' /etc/bashrc /etc/profile

cat << EOF > /etc/skel/.bash_profile
umask 0077
EOF
