import { KVData } from '../node-credentials/node-credential.model';

export interface Destination {
  id: string;
  name: string;
  url: string;
  secret: string;
  enable?: boolean;
  integration_types?: string;
  meta_data?: Array<KVData>;
  services?: string;
}



export interface EnableDestination {
  id: string;
  enable: boolean;
}
