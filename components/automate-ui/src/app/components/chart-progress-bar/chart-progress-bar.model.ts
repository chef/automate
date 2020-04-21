export interface Styles {
  bar?: Bar;
  container?: Container;
  progress?: Progress;
  threshold?: any;
}

export interface Bar {
  background?: string;
  borderRadius?: string;
  boxShadow?: string;
  height?: string;
  width?: string;
}

export interface Container {
  height?: string;
}

export interface Progress {
  background?: string;
  borderRadius?: string;
  bordeTopRightRadius?: string;
  bordeBottomRightRadius?: string;
  height?: string;
  width?: string;
}

export interface Threshold {
  point1?: Point;
  point2?: Point;
}

export interface Point {
  height?: string;
  marginLeft?: string;
}
