+++
title = "Airgapped HA Installation"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Airgapped HA Installation"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha_airgap.md Airgapped HA Installation"
    weight = 270
+++

An airgapped host means it has no direct inbound or outbound access to internet connectivity. You must create an Airgap Installation Bundle,`.aib` on an internet-connected host, to install or upgrade Chef Automate on an airgapped host. Then, transfer both the Airgap installation bundle and the chef-automate binary that you used to create it to the airgapped host for use.

## License

To obtain a trial license for an airgapped host [contact Chef](https://www.chef.io/contact-us).

## Airgap Installation Bundle

Follow these steps to prepare an airgap installation bundle:

1. Execute `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate` command. It downloads the Chef Automate command-line tool from the current [release channel](https://docs.chef.io/automate/install/#release-channels) on an internet-connected host.

1. Execute `./chef-automate airgap bundle create` command. It downloads and bundles the software named `automate-<timestamp>.aib` in bastion host.

{{< note >}}

You can download and bundle the software included in a specific Chef Automate release by executing the command `./chef-automate airgap bundle create --version VERSION`.

{{< /note >}}

1. Execute `scp -i your-private-key.pem airgap-bundle.aib user@destination-ip-addess-172-32-0-1:airgap-bundle.aib scp -i your-private-key.pem chef-automate user@destination-ip-addess-172-32-0-1:chef-automate` command. It copies the airgap bundle and the chef-automate cli on your non-internet environment.

1. Execute `chmod +x chef-automate` command. It provides then executable permision to chef-automate cli.

1. Login to your non-internet instance where you have copied airgap bundle.

1. Generate `config.toml` using command `./chef-automate init-config-ha existing_infra`.

1. Open `config.toml` in your preferred editor.

1. Specify following details, `ssh_user`, `ssh_key_file`, `fqdn`, `instance_count`, `automate_private_ip` and other IPs. See [Configuring Chef Automate](https://docs.chef.io/automate/configuration/) for more information on configuration settings.

1. Set **SElinux** configuration as *enforcing* to *permissive* in all the nodes if your instance is `Redhat`. To do so, execute the command `sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config` and SSH (login securely) into your instance where you want to set the *SElinux config.Reboot*.

1. Deploy the airgap bundle using the command `./chef-automate deploy </path/to/config.toml> --airgap-bundle </path/to/airgap-bundle>`, where you need to provide the path of the `cofig.toml` and `arirgap bundle` files in </path/to/config.toml> and </path/to/airgap-bundle>.

  Deploying Chef Automate takes ten minutes for a clean install. At the command prompt, accept the terms of service with a `y`. The installer then performs a series of pre-flight checks. Any unsuccessful checks offer information for resolving issues or skipping the check. After resolving any pre-flight issues, run the deploy command.

  At the end of the deployment process, you will see, `Deploy complete`.

  The deployment process writes login credentials to the `automate-credentials.toml` in your current working directory.

1. Navigate to `https://automate.example.com` in a browser and log in to Chef Automate with the credentials provided in *automate-credentials.toml*. Once you log in, Chef Automate prompts you for a license.
