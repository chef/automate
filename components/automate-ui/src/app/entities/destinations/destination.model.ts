export interface Destination {
  id: string;
  name: string;
  url: string;
  secret: string;
  enable?: boolean;
  integration_type?: string;
  meta_data?: string;
  services?: string;
}
