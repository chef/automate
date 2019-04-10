export interface Job {
  id: string;
  name: string;
  node_count: number;
  tags: {};
  type: string;
  status: string;
  start_time: string;
  end_time: string;
}

export interface Node {
  id: string;
  name: string;
  platform: string;
  platform_version: string;
  last_contact: string;
  manager: string;
  environment: string;
  reporter: string;
  status: string;
  tags: {};
}

export interface StatusTotals {
  all: number;
  unreachable: number;
  reachable: number;
}
