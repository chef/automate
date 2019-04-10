export interface FilterA {
  type?: string;
  value?: string;
}

export interface FilterB {
  type?: {name?: string};
  value: string;
}

export interface FilterC {
  type?: {name?: string};
  value: {text?: string, id?: string};
}

export type Filter = FilterA | FilterB | FilterC;
