export interface FilterOption {
  name: FilterName;
  id: string;
}

export interface FilterUpdate {
  filters: string[];
  saveAsDefault: boolean;
}

export enum FilterName {
  PlatformVersion = 'Platform Version',
  Domain = 'Domain',
  Tag = 'Tag',
  ChefVersion = 'Chef Version',
  IpAddress = 'IP address',
  Ip6Address = 'IP 6 address',
  MacAddress = 'MAC address',
  Uptime = 'Uptime',
  MemoryTotal = 'Memory Total',
  VirtualizationSystem = 'Virtualization System',
  VirtualizationRole = 'Virtualization Role',
  KernelRelease = 'Kernel Release',
  KernelVersion = 'Kernel Version',
  Hostname = 'Hostname',
  Timezone = 'Timezone',
  LastCheckInTime = 'Last Check-in Time',
  DMIsystemManufacturer = 'DMI System Manufacturer',
  DMIsystemSerialNumber = 'DMI System Serial Number',
  CloudProvider = 'Cloud Provider'
}

export const FilterableOptions: FilterOption[] = [
  { name: FilterName.PlatformVersion, id: 'platform_version'},
  { name: FilterName.VirtualizationRole, id: 'virtualization_role'},
  { name: FilterName.Domain, id: 'domain'},
  { name: FilterName.KernelRelease, id: 'kernel_release'},
  { name: FilterName.Tag, id: 'tag'},
  { name: FilterName.KernelVersion, id: 'kernel_version'},
  { name: FilterName.ChefVersion, id: 'chef_version'},
  { name: FilterName.Hostname, id: 'hostname'},
  { name: FilterName.IpAddress, id: 'ip_address'},
  { name: FilterName.Timezone, id: 'timezone'},
  { name: FilterName.Ip6Address, id: 'ip_6_address'},
  { name: FilterName.LastCheckInTime, id: 'last_check_in_time'},
  { name: FilterName.MacAddress, id: 'mac_address'},
  { name: FilterName.DMIsystemManufacturer, id: 'dmi_system_manufacturer'},
  { name: FilterName.Uptime, id: 'uptime'},
  { name: FilterName.DMIsystemSerialNumber, id: 'dmi_system_serial_number'},
  { name: FilterName.MemoryTotal, id: 'memory_total'},
  { name: FilterName.CloudProvider, id: 'cloud_provider'},
  { name: FilterName.VirtualizationSystem, id: 'virtualization_system'}
];
