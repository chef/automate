export interface Server {
  id: string;
  name: string;
  fqdn: string;
  ip_address: string;
  orgs_count?: number;
}
