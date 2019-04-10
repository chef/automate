# Import all plugins from `rel/plugins`
# They can then be used by adding `plugin MyPlugin` to
# either an environment, or release definition, where
# `MyPlugin` is the name of the plugin module.
Path.join(["rel", "plugins", "*.exs"])
|> Path.wildcard()
|> Enum.map(&Code.eval_file(&1))

use Mix.Releases.Config,
    # This sets the default release built by `mix release`
    default_release: :default,
    # This sets the default environment used by `mix release`
    default_environment: Mix.env()

environment :dev do
  set dev_mode: true
  set include_erts: false
  set cookie: :"IZ[LX<u1$j.<au$jnD5i]~uqA&p}kk@b||@>mSbd|adN&<wDV>NL9^?,^6wChc!`"
end

environment :habitat do
  set include_erts: true
  set include_src: false
  set vm_args: "./rel/vm.args"
  set cookie: :"IZ[LX<u1$j.<au$jnD5i]~uqA&p}kk@b||@>mSbd|adN&<wDV>NL9^?,^6wChc!`"
end

# You may define one or more releases in this file.
# If you have not set a default release, or selected one
# when running `mix release`, the first release in the file
# will be used by default

release :notifications do
  set version: String.trim(File.read!("../VERSION"))
  set applications: [
    :runtime_tools,
    notifications: :permanent
  ]
end
