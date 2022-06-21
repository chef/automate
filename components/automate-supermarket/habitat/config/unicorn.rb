##
# Unicorn config at supermarket
# Managed by Chef - Local Changes will be Nuked from Orbit (just to be sure)
##

# What ports/sockets to listen on, and what options for them.
listen "127.0.0.1:{{ cfg.app.port }}"

# What the timeout for killing busy workers is, in seconds
timeout {{ cfg.app.worker_timeout }}

# Whether the app should be pre-loaded
preload_app {{ cfg.app.preload_app }}

# How many worker processes
worker_processes {{ cfg.app.worker_processes }}

# Run forked children as specified user/group
user "{{ cfg.app.user }}", "{{ cfg.app.group }}"

# Where to drop a pidfile
pid '{{ pkg.svc_var_path }}/unicorn.pid'

# http://www.rubyenterpriseedition.com/faq.html#adapt_apps_for_cow
if GC.respond_to?(:copy_on_write_friendly=)
  GC.copy_on_write_friendly = true
end
