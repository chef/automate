export interface Node {
  id: string;
  name: string;
}

export interface NodeTotals {
  all: number;
  unreachable: number;
  reachable: number;
  unknown: number;
}
