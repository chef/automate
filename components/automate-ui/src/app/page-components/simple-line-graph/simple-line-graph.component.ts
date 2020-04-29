import {
  Component, Input, OnChanges, ViewChild, ElementRef, OnInit
} from '@angular/core';
import * as d3 from 'd3';
// import * as moment from 'moment';
// import { DateTime } from 'app/helpers/datetime/datetime';


@Component({
  selector: 'app-simple-line-graph',
  templateUrl: './simple-line-graph.component.html',
  styleUrls: ['./simple-line-graph.component.scss']
})

export class SimpleLineGraphComponent implements OnChanges, OnInit {

  constructor(
    private chart: ElementRef
  ) {}

  // using onInit for fake data
  @Input() data: any = [
    { daysAgo: 6, percentage: 37 },
    { daysAgo: 5, percentage: 27 },
    { daysAgo: 4, percentage: 67 },
    { daysAgo: 3, percentage: 77 },
    { daysAgo: 2, percentage: 87 },
    { daysAgo: 1, percentage: 97 },
    { daysAgo: 0, percentage: 68 }
  ];
  @Input() width = 900;
  @Input() height = 300;

  @ViewChild('svg', {static: true}) svg: ElementRef;


  ////////   X AXIS ITEMS   ////////
  // maps all of our x data points
  get xData() {
    return this.data.map(d => d.daysAgo);
  }
  // determines how wide the graph should be to hold our data
  // in its respective area;
  get rangeX() {
    const min = 50;
    const max = this.width - 50;
    return [min, max];
  }
  // determines the min and max values of the x axis
  get domainX() {
    const min = Math.min(...this.xData);
    const max = Math.max(...this.xData);
    return [min, max];
  }
  // determines each of our X axis points using the height and width of the chart
  get xScale() {
    return d3.scaleTime()
      .range(this.rangeX)
      .domain(this.domainX);
  }


  ////////   Y AXIS ITEMS   ////////

  // maps all of our Y data points
  get yData() {
    return this.data.map(d => d.percentage);
  }

  get rangeY() {
    const min = 10;
    const max = this.height - 10;
    return [max, min];
  }

  get domainY() {
    const min = 0;
    const max = 100; // since this based on a percentage do we want 0 to 100?
    return [min, max];
  }
  // determines each of our Y axis points using the height and width of the chart
  get yScale() {
    return d3.scaleLinear()
             .range(this.rangeY)
             .domain(this.domainY);
  }

  //////// SELECTIONS ////////
  get axisYSelection() {
    return this.svgSelection.select('.y-axis');
  }

  get svgSelection() {
    return d3.select(this.svg.nativeElement);
  }

  // returns a function that when passed our data, will return an svg path
  get path() {
    return d3.line()
              .x(d => this.xScale( d.daysAgo) )
              .y(d => this.yScale( d.percentage) );
  }


  get viewBox() {
    return `0 0 ${this.width} ${this.height}`;
  }

  resizeChart() {
    this.width = this.chart.nativeElement.getBoundingClientRect().width;
  }

  drawLine() {
    // create the line using path function
    const line = this.path(this.data);
    
    // append the line to the chart
    this.svgSelection.append('path')
                      .attr('class', 'line')
                      .attr('d', line);
  }

  drawChartLines() {
    const yAxis = d3.axisRight(this.yScale).ticks(1);
    this.axisYSelection.call(yAxis);

    // draw the x axis grid lines
    const xGrid = d3.axisTop()
                      .ticks(this.data.length)
                      .tickFormat('')
                      .tickSize(this.height - 20) // will need to subtract extra height here
                      .tickSizeOuter(0)
                      .scale(this.xScale);

    this.svgSelection.append('g')
                 .attr('class', 'grid')
                 // this line will need to be updated to flexible
                 .attr('transform', `translate(0, ${this.height - 10})`)
                 .call(xGrid);

    // remove zero from bottom of chart on x axis
    this.svgSelection.selectAll('.tick')
                  .filter(t => t === 0)
                  .remove();
  }

  ngOnChanges() {
    // console.log(this.data);
    // this.resizeChart();
    // this.drawChartLines();
    // this.drawLine();
  }

  ngOnInit() {
    console.log(this.data);
    this.resizeChart();
    this.drawChartLines();
    this.drawLine();
  }

  onResize() {
    this.resizeChart();
  }
}
