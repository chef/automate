export interface PolicyFile {
  name: string;
  revision_id: string;
  policy_group: string;
  cookbook_locks?: CookbookLocks[];
  included_policy_locks?: IncludedPolicyLocks[];
  default_attributes?: string;
  override_attributes?: string;
  solution_dependecies?: SolutionDependencies[];
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

export interface SolutionDependencies {
  name: string;
  version: string;
  dependencies: Dependencies[];
}

export interface Dependencies {
  name: string;
  version: string;
}

export interface PolicyGroup {
  name: string;
  policies: IncludedPolicyLocks[];
  uri: string;
}
