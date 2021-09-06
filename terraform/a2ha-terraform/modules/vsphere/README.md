# automate-backend-vsphere-module

Terraform Module

## Module Dependencies
None

## Description
Creates a specified number of servers labelled as Postgres, Elasticsearch and Automate on your vSphere cluster.
- It does no configuration of the machines, except for mounting and formatting the data disk
- It expects the vSphere cluster to have HA and DRS enabled, rather than selecting compute and storage resources for each VM.

## Example usage
Here's how you would consume this module from a top-level terraform plan:

```
module "vsphere" {
  source                              = "./modules/vsphere"
  tag_name                            = "A2"
  vsphere_linux_sshuser               = "vagrant"
  vsphere_linux_sshkeyfile            = "~/.ssh/vagrant"
  vsphere_user                        = "administrator@success.chef.co"
  vsphere_password                    = "My Secure Password"
  vsphere_server                      = "vcenter.success.chef.co"
  vsphere_allow_unverified_ssl        = true
  vsphere_datacenter                  = "Tierpoint"
  vsphere_datastore                   = "FreeNAS03 iSCSI"
  vsphere_resource_pool               = "a2-ha-backend"
  vsphere_network                     = "VM Network"
  vsphere_linux_template              = "centos76-template"
  postgresql_instance_count           = "3"
  postgresql_instance_cpus            = "2"
  postgresql_instance_ram_mb          = "4096"
  postgresql_instance_datadisk_gb     = "20"
  elasticsearch_instance_count        = "3"
  elasticsearch_instance_cpus         = "2"
  elasticsearch_instance_ram_mb       = "8192"
  elasticsearch_instance_datadisk_gb  = "100"
  automate_instance_count             = "1"
  automate_instance_cpus              = "2"
  automate_instance_ram_mb            = "8192"
  automate_instance_datadisk_gb       = "100"
}
```
