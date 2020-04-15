export interface Lists {
  name: string;
  version: string;
  type: string;
  skipped: string;
  children: ChildLists[];
}

export interface ChildLists {
  name: string;
  version: string;
  type: string;
}
