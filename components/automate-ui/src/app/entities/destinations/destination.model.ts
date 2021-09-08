export interface Destination {
  id: string;
  name: string;
  url: string;
  secret: string;
  enable: boolean;
  services: string;
  integration_types: string;
  meta_data: string[];
}

export interface EnableDestination {
  id: string;
  enable: boolean;
}

