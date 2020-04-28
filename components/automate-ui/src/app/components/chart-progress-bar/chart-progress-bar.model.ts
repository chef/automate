export interface Styles {
  bar?: Bar;
  progress?: Progress;
  threshold?: Threshold;
}

export interface Bar {
  width?: string;
}

export interface Progress {
  width?: string;
}

export interface Threshold {
  [index: string]: { marginLeft?: string };
}
