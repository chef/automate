export interface ContentItem {
  id: string;
  name: string;
  type: string;
  description: string;
  version: string;
  platforms: string[];
  canBeInstall: boolean;
  filename: string;
}

export interface Credentials {
  clientId: string;
  clientSecret: string;
  tenantSpecificUrl: string;
}
