import { Runlist } from 'app/entities/runlists/runlists.model';

export const mockExpandedTree: Runlist[] = [
{
  id: 'chef-environment-924962200',
  run_list: [{
    type: 'recipe',
    name: 'chef-load-role-580613600',
    version: null,
    position: null,
    missing: true,
    no_version: false,
    skipped: false
  }]
},
{
  id: '_default',
  run_list: [
    {
      type: 'role',
      name: 'chef-load-role-580613600',
      children: [],
      missing: null,
      error: '404 \'Object Not Found\'',
      skipped: null
    },
    {
      type: 'role',
      name: 'chef-load-role-37386300',
      children: [
        {
          type: 'role',
          name: 'chef-load-role-159662700',
          children: [],
          missing: null,
          error: null,
          skipped: null
        },
        {
          type: 'recipe',
          name: 'aix::nim_master_setup',
          version: '2.3.12',
          position: 0,
          missing: false,
          no_version: false,
          skipped: false
        },
        {
          type: 'recipe',
          name: 'aix::nim_master_setup_standalone',
          version: '',
          position: 1,
          missing: false,
          no_version: false,
          skipped: false
        },
        {
          type: 'role',
          name: 'chef-load-role-228526900',
          children: [],
          missing: null,
          error: null,
          skipped: null
        },
        {
          type: 'recipe',
          name: 'audit::inspec',
          version: '9.5.1',
          position: 2,
          missing: false,
          no_version: false,
          skipped: false
        },
        {
          type: 'recipe',
          name: 'centos-cookbook-file',
          version: '0.1.0',
          position: 3,
          missing: false,
          no_version: false,
          skipped: false
        },
        {
          type: 'role',
          name: 'chef-load-role-570863400',
          children: [],
          missing: null,
          error: null,
          skipped: null
        },
        {
          type: 'role',
          name: 'a',
          children: [],
          missing: null,
          error: null,
          skipped: null
        },
        {
          type: 'role',
          name: 'chef-load-role-580613600',
          children: [],
          missing: null,
          error: '404 \'Object Not Found\'',
          skipped: true
        }],
      missing: null,
      error: null,
      skipped: null
    },
    {
      type: 'recipe',
      name: 'aix::nim_master_setup',
      version: '2.3.12',
      position: null,
      missing: false,
      no_version: false,
      skipped: true
    }]
}];
