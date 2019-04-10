export interface ChartDataElement {
  name: string;
  value: number;
}

export interface BubbleChartDataElement {
  name: string;
  series: Array<BubbleChartSeriesElement>;
}

export interface BubbleChartSeriesElement {
  name: string;
  x: string;
  y: string;
  r: string;
}

export interface OrdinalScale {
  domain: Array<string>;
}
