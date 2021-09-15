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

export const regions = [
  { name: 'US East (Ohio) us-east-2', code: 'us-east-2' },
  { name: 'US East (N. Virginia) us-east-1', code: 'us-east-1' },
  { name: 'US West (N. California) us-west-1', code: 'us-west-1' },
  { name: 'US West (Oregon) us-west-2', code: 'us-west-2' },
  { name: 'Africa (Cape Town) af-south-1', code: 'af-south-1' },
  { name: 'Asia Pacific (Hong Kong) ap-east-1', code: 'ap-east-1' },
  { name: 'Asia Pacific (Mumbai) ap-south-1', code: 'ap-south-1' },
  { name: 'Asia Pacific (Osaka) ap-northeast-3', code: 'ap-northeast-3' },
  { name: 'Asia Pacific (Seoul) ap-northeast-2', code: 'ap-northeast-2' },
  { name: 'Asia Pacific (Singapore) ap-northeast-2', code: 'ap-northeast-2' },
  { name: 'Asia Pacific (Sydney) ap-southeast-2', code: 'ap-southeast-2' },
  { name: 'Asia Pacific (Tokyo) ap-northeast-1', code: 'ap-northeast-1' },
  { name: 'Canada (Central) ca-central-1', code: 'ca-central-1' },
  { name: 'China (Beijing) cn-north-1', code: 'cn-north-1' },
  { name: 'China (Ningxia) cn-northwest-1', code: 'cn-northwest-1' },
  { name: 'Europe (Frankfurt) eu-central-1', code: 'eu-central-1' },
  { name: 'Europe (Ireland) eu-west-1', code: 'eu-west-1' },
  { name: 'Europe (London) eu-west-2', code: 'eu-west-2' },
  { name: 'Europe (Milan) eu-south-1', code: 'eu-south-1' },
  { name: 'Europe (Paris) eu-west-3', code: 'eu-west-3' },
  { name: 'Europe (Stockholm) eu-north-1', code: 'eu-north-1' },
  { name: 'Middle East (Bahrain) me-south-1', code: 'me-south-1' },
  { name: 'South America (São Paulo) sa-east-1', code: 'sa-east-1' }
];
