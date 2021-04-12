export interface Runlist {
  id: string;
  run_list: List[];
}

export interface List {
  type: string;
  name: string;
  version?: string;
  position?: number;
  missing?: boolean;
  error?: string;
  no_version?: boolean;
  skipped: boolean;
  children?: List[];
}

export interface ExpandedChildList {
  type: string;
  name: string;
  version?: string;
  position?: number;
  error?: string;
  missing?: boolean;
  no_version?: boolean;
  skipped: boolean;
}
