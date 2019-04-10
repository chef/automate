# Developing with A2 and On-Prem Builder

This guide describes how to stand up an instance of Automate (A2) that
can talk to a running instance of On-Prem Builder (Builder)
primarily for the purpose of allowing Builder to use A2 to authenticate.

Note: This development guide follows the [Vagrant dev env](https://github.com/chef/automate/blob/master/dev-docs/DEV_ENVIRONMENT.md#vagrant-setup)
pattern.

## Set up your host

1. Clone the [On-Prem Builder](https://github.com/habitat-sh/on-prem-builder) repository
   into your `$GOPATH/src/github.com/chef` directory.

2. Create a Vagrantfile in that new `on-prem-builder` folder
   (_but don't commit this file!_) with this content:

   ```ruby
   Vagrant.configure("2") do |config|

     config.vm.box = "bento/ubuntu-16.04"

     config.vm.provider "virtualbox" do |v|
       v.memory = 4096
       v.cpus = 2
     end

     config.vm.synced_folder "~/.hab", "/home/vagrant/.hab"

     config.vm.define "builder" do |builder|
       builder.vm.network "forwarded_port", guest: 9000, host: 9000
       builder.vm.hostname = 'builder.test'
      builder.vm.network 'private_network', ip: '192.168.33.223'
     end
   end
   ```

3. Edit your /etc/hosts file (with e.g. `sudo vi /etc/hosts`).
   Add (or update) entries for Builder and for A2.

   ```text
   192.168.33.222 a2-dev.test
   192.168.33.223 builder.test
   ```

   The IP address for `builder.test` comes from the Vagrantfile definition in step 2.
   Similarly, the IP address for `a2-dev.test` must match the pre-existing Vagrantfile
   in your `a2` root.

## Setting up your Automate VM

1. Switch directories to your `a2` root and execute `vagrant up`.
   That boots the VM defined in A2's Vagrantfile.
   You may want to `vagrant destroy` first to start fresh.

2. Execute `vagrant ssh -- -R 4200:localhost:4200` to connect to the VM and get a command prompt.
   That actually spins up habitat studio, so the command prompt is that of the hab studio
   running on the VM.

3. Before starting A2 services, exit hab studio (using `ctrl+D`) to finish one more setup step.

4. Once outside the studio, add Builder's IP address and hostname to the `/etc/hosts`
   of this VM, just as you did with your host.
   It is advised to test the connection, but the machine does not yet exist.
   So this step is deferred until you set up the Builder VM.

5. Go back into the studio with `hab studio enter`.

6. (Optional) If you plan to build A2 's UI locally, execute
   `build components/automate-ui-devproxy/`.

7. Execute `start_all_services` to launch A2. This step may take a few minutes.

8. Once A2 is running, apply a configuration patch to set the Builder redirect URL,
   Oauth2 client ID, and client secret, all of which are empty by default.
   Create a `session.toml` file with this content:

    ```toml
    [session.v1.sys.service]
    bldr_signin_url = "http://builder.test/"
    bldr_client_id = "testclient"
    bldr_client_secret = "mysupersecret"
    ```

    Run `chef-automate config patch session.toml` to deploy your change.

## Setting up your Builder VM

1. In a separate terminal tab or window, switch directories to the `on-prem-builder` root
   and execute `vagrant up` to boot the VM for Builder.

2. Execute `vagrant ssh builder` to connect to the VM and get a command prompt.
   In contrast to the A2 VM, here you will get a standard Linux prompt.

3. Add A2's IP address and hostname to the `/etc/hosts`
   of this VM, just as you did with your host.
   Test the connection to A2 with `ping a2-dev.test`.

4. This is a good point to test the connection in the _other_ direction as well,
   now that the Builder VM exists.
   Switch to your terminal tab or window for the A2 VM, then run
   `install_if_missing core/busybox ping` to setup ping for use.
   Test the connection to Builder with `ping builder.test`.

5. Switch directories with `cd /vagrant`.

6. Copy the sample environment file with `cp bldr.env.sample bldr.env`
   and then open `bldr.env` for editing. Replace the following values:

   ```text
   export APP_URL=http://builder.test

   export OAUTH_PROVIDER=chef-automate

   export OAUTH_USERINFO_URL=https://a2-dev.test/session/userinfo

   export OAUTH_AUTHORIZE_URL=https://a2-dev.test/session/new

   export OAUTH_TOKEN_URL=https://a2-dev.test/session/token

   export OAUTH_REDIRECT_URL=http://builder.test/

   export OAUTH_CLIENT_ID=testclient

   export OAUTH_CLIENT_SECRET=mysupersecret
   ```

7. Begin the installation with `sudo ./install.sh`.
    Note: if it aborts (e.g. with "Connection refused") or you need
    to re-install for any other reason,
    first `sudo ./uninstall.sh` then run `sudo ./install.sh` again.

8. In order for A2 to pass Builder's certificate verification you need
   to install A2's public certificate on Builder.
   Locate the public certificate from A2's `dev/config.toml`.
   Copy the `cert` value inside the
   `load_balancer.v1.sys.frontend_tls` config section.
   Add this cert to Builder's list of certificates (e.g. using
  `sudo vi /hab/pkgs/core/cacerts/2018.03.07/20180608102212/ssl/cert.pem`)
   by pasting the certificate at the end of the file.

9. With both A2 and Builder now up and running, open `http://builder.test`
   in your browser and you should be able to log in with the credentials
   of any Automate user. This always includes local users and may include
   others, depending on what identity providers you have setup (e.g. LDAP).
