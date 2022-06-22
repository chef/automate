export interface NodeRunlist {
  id: string;
  run_list: NodeList[];
}

export interface NodeList {
  type: string;
  name: string;
  version?: string;
  position?: number;
  missing?: boolean;
  error?: string;
  no_version?: boolean;
  skipped: boolean;
  children?: NodeList[];
}

export interface NodeExpandedChildList {
  type: string;
  name: string;
  version?: string;
  position?: number;
  error?: string;
  missing?: boolean;
  no_version?: boolean;
  skipped: boolean;
}
