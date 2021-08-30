export interface Destination {
  id: string;
  name: string;
  url: string;
  secret: string;
  enable?: boolean;
  integration_types?: string;
  meta_data?: string;
  services?: string;
}
