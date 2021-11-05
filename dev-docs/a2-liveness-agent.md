## Automate Liveness Agent

Automate liveness agent sends keepalive messages to Chef Automate, 
which prevents nodes that are up but not frequently running Chef Client 
from appearing as "missing" in Automate UI.  
The default interval of sending the keepalive messages is 1800 seconds.

### Repository
https://github.com/chef/automate-liveness-agent

### Supported Platforms   

- aix  
- amazon  
- debian  
- fedora  
- freebsd  
- mac_os_x  
- oracle  
- rhel  
- solaris2  
- suse  
- windows