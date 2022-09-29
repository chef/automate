# Runs PowerShell commands as elevated over Windows Remote Management (WinRM) via a scheduled task
[![Gem Version](https://badge.fury.io/rb/winrm-elevated.svg)](http://badge.fury.io/rb/winrm-elevated)

This gem allows you to break out of the magical WinRM constraints thus allowing to reach out to network shares and even install Windows updates, .NET, SQL Server etc.

## Running commands elevated
```ruby
require 'winrm'
require 'winrm-elevated'

conn = WinRM::Connection.new(...
conn.shell(:elevated) do |shell|
  shell.run('$PSVersionTable') do |stdout, stderr|
    STDOUT.print stdout
    STDERR.print stderr
  end
end
```

### Impersonating a service account
By passing a `nil` password, winrm-elevated will assume that the command should run as a service account:
```ruby
require 'winrm'
require 'winrm-elevated'

conn = WinRM::Connection.new(...
conn.shell(:elevated) do |shell|
  shell.username = 'System'
  shell.password = nil
  shell.run('$PSVersionTable') do |stdout, stderr|
    STDOUT.print stdout
    STDERR.print stderr
  end
end
```

### Using an interactive task
By setting `interactive_logon` to `true`, the scheduled task will be configured to use an interactive logon allowing all command activity to be viewable from a RDP session if logged on as the same user as the winrm credentials:
```ruby
require 'winrm'
require 'winrm-elevated'

conn = WinRM::Connection.new(...
conn.shell(:elevated) do |shell|
  shell.interactive_logon = true
  shell.run('notepad.exe')
end
```

## How does it work?

The gem works by creating a new logon session local to the Windows box by using a scheduled task. After this point WinRM is just used to read output from the scheduled task via a log file.

1. The command you'd like to run outside the WinRM context is saved to a temporary file.
2. This file is uploaded to the machine over WinRM.
3. A script is executed over WinRM and does the following:
  1. Scheduled task is created which will execute your command and redirect stdout and stderr to a location known by elevated_shell.ps1.
  2. The scheduled task is executed.
  3. elevated_shell.ps1 polls the stdout and stderr log files and writes them back to WinRM. The script continues in this loop until the scheduled task is complete.

## Troubleshooting

If you're having trouble, first of all its most likely a network or WinRM configuration
issue. Take a look at the [WinRM gem troubleshooting](https://github.com/WinRb/WinRM#troubleshooting)
first.

## Contributing

1. Fork it.
2. Create a branch (git checkout -b my_feature_branch)
3. Run the unit and integration tests (bundle exec rake integration)
4. Commit your changes (git commit -am "Added a sweet feature")
5. Push to the branch (git push origin my_feature_branch)
6. Create a pull requst from your branch into master (Please be sure to provide enough detail for us to cipher what this change is doing)

### Running the tests

We use Bundler to manage dependencies during development.

```
$ bundle install
```

Once you have the dependencies, you can run the unit tests with `rake`:

```
$ bundle exec rake spec
```

To run the integration tests you will need a Windows box with the WinRM service properly configured. Its easiest to use the Vagrant Windows box in the Vagrantilfe of this repo.

1. Create a Windows VM with WinRM configured (see above).
2. Copy the config-example.yml to config.yml - edit this file with your WinRM connection details.
3. Run `bundle exec rake integration`

## WinRM-elevated Authors
* Shawn Neal (https://github.com/sneal)

[Contributors](https://github.com/WinRb/winrm-elevated/graphs/contributors)
