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
  // @Input() data: any = []
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

  // for temp fake data -- delete isData1
  public isData1 = false;



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

  renderChart() {
    const t = d3.transition().duration(1000);

    // create the line using path function
    const line = this.path(this.data);

    // make Update selection for any incoming data
    const update = this.svgSelection
      .selectAll('.line')
      .data(this.data.filter(d => d.daysAgo));

    // remove any points no longer needed
    update
      .exit()
      .remove();

    // make enter selection
    // this is for any new data coming in
    // const enter = update
    //   .enter()
    //   .append('path');
    // update
    //   .transition()
    //   .delay(1000);

    //  merge new data with existing data
    update
      .enter()
      .append('path')
      .attr('class', 'line')
      .merge(update)
      .transition(t)
      .attr('d', line);
  }


  ////////////////// RENDER FUNCTIONS ////////////////////
  renderGrid() {
    // create the Y axis
    const yAxis = d3.axisRight(this.yScale)
                    .tickFormat(d => d + '%')
                    .ticks(1);
    // create the X axis grid lines
    const xGrid = d3.axisTop()
      .ticks(this.data.length)
      .tickFormat('')
      .tickSize(this.height - 20) // will need to subtract extra height here
      .tickSizeOuter(0)
      .scale(this.xScale);

    // Add the Y Axis
    this.svgSelection
      .append('g')
      .attr('class', 'y-axis')
      .call(yAxis);

    // Add the Grid lines
    this.svgSelection
      .append('g')
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
    this.resizeChart();
    this.renderGrid();
    this.renderChart();
  }

  ngOnInit() {
    console.log(this.data);
    // this.resizeChart();

    // this.renderGrid();
    // this.renderChart();
  }

  onResize() {
    this.resizeChart();
  }


  /// TEMP FAKE DATA
  changeData() {
    const data = [
        { daysAgo: 6, percentage: 37 },
        { daysAgo: 5, percentage: 27 },
        { daysAgo: 4, percentage: 67 },
        { daysAgo: 3, percentage: 77 },
        { daysAgo: 2, percentage: 87 },
        { daysAgo: 1, percentage: 97 },
        { daysAgo: 0, percentage: 68 }
      ];

    const data1 = [
      { daysAgo: 2, percentage: 22 },
      { daysAgo: 1, percentage: 33 },
      { daysAgo: 0, percentage: 88 }
    ];

    if (this.isData1) {
      this.data = data;
      this.isData1 = false;
    } else {
      this.data = data1;
      this.isData1 = true;
    }

    this.renderGrid();
    this.renderChart();
  }
}
