import {
  Component, Input, OnChanges, ViewChild, ElementRef
} from '@angular/core';
import * as d3 from 'd3';
// import * as moment from 'moment';
// import { DateTime } from 'app/helpers/datetime/datetime';


@Component({
  selector: 'app-simple-line-graph',
  templateUrl: './simple-line-graph.component.html',
  styleUrls: ['./simple-line-graph.component.scss']
})

export class SimpleLineGraphComponent implements OnChanges {

  constructor(
    private chart: ElementRef
  ) {}

  @Input() data: any = [];
  @Input() width = 900;
  @Input() height = 300;

  @ViewChild('svg', {static: true}) svg: ElementRef;


  // maps all of our x data points
  get xData() {
    return this.data.map(d => d.daysAgo);
  }

  // maps all of our Y data points
  get yData() {
    return this.data.map(d => d.percentage);
  }

  // determines how wide the graph should be to hold our data
  // in its respective area;
  get rangeX() {
    const min = 50;
    const max = this.width - 50;
    return [min, max];
  }

  get rangeY() {
    const min = 10;
    const max = this.height - 10;
    return [max, min];
  }

  // determines the min and max values of the x axis
  get domainX() {
    const min = Math.min(...this.xData);
    const max = Math.max(...this.xData);
    return [min, max];
  }

  get domainY() {
    const min = 0;
    const max = 100; // since this based on a percentage do we want 0 to 100?
    return [min, max];
  }

  // determines each of our X axis points using the height and width of the chart
  get scaleX() {
    return d3.scaleLinear()
              .range(this.rangeX)
              .domain(this.domainX);
  }

  // determines each of our Y axis points using the height and width of the chart
  get scaleY() {
    return d3.scaleLinear()
             .range(this.rangeY)
             .domain(this.domainY);
  }

  get axisYSelection() {
    return this.chartSvg.select('.y-axis');
  }

  // returns a function that when passed our data, will return an svg path
  get path() {
    return d3.line()
              .x(d => this.scaleX( d.daysAgo) )
              .y(d => this.scaleY( d.percentage) );
  }

  get chartSvg() {
    return d3.select(this.svg.nativeElement);
  }

  get viewBox() {
    return `0 0 ${this.width} ${this.height}`;
  }

  resizeChart() {
    this.width = this.chart.nativeElement.getBoundingClientRect().width;
  }

  drawLine() {
    // we are looking at days backwards, so we need the data reversed;
    const reversedData = this.data.reverse();

    // in order to redraw lines, we'll have to use enter and exit
    const path = this.chartSvg.append('path')
                              .attr('class', 'line');
    path.attr('d', this.path(reversedData));
  }

  drawChartLines() {
    const yAxis = d3.axisRight(this.scaleY).ticks(1);
    this.axisYSelection.call(yAxis);

    // draw the x axis grid lines
    const grid = d3.axisTop()
                      .ticks(this.data.length)
                      .tickFormat('')
                      .tickSize(this.height) // will need to subtract extra height here
                      .scale(this.scaleX);

    this.chartSvg.append('g')
                 .attr('class', 'grid')
                 // this line will need to be updated to flexible
                 .attr('transform', `translate(0, ${this.height - 10})`)
                 .call(grid);

    // remove zero from bottom of chart on x axis
    this.chartSvg.selectAll('.tick')
                  .filter(t => t === 0)
                  .remove();
  }

  ngOnChanges() {
    console.log(this.data);
    this.resizeChart();
    this.drawChartLines();
    this.drawLine();
  }

  onResize() {
    this.resizeChart();
  }
}
