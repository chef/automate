export interface List {
  name: string;
  version: string;
  type: string;
  skipped: string;
  children: ChildList[];
}

export interface ChildList {
  name: string;
  version: string;
  type: string;
}
