export interface GlobalConfig {
  feed_interval: string;
  node_batch_size: string;
  updated_nodes_only: boolean;
  disable_cidr_filter: boolean;
  cidr_filter: string[];
  accepted_status_codes: number[];
}
