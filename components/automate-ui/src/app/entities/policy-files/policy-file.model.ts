export interface PolicyFile {
  name: string;
  revision_id: string;
  policy_group: string;
  cookbook_locks?: CookbookLocks[];
  included_policy_locks?: IncludedPolicyLocks[];
}

export interface CookbookLocks {
  name: string;
  version?: string;
  source?: string;
}

export interface IncludedPolicyLocks {
  name: string;
  revision_id: string;
}
