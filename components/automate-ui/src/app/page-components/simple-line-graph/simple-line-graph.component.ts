import {
  Component, Input, OnChanges, ViewChild, ElementRef
} from '@angular/core';
import * as d3 from 'd3';


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
  @Input() height = 200;

  @ViewChild('svg', {static: true}) svg: ElementRef;

  public heightMargin = 40;
  public theHighlight;
  public locked = false;
  public currentHighlightnum: number;

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
    return [max, min];  // we want to plot our data backwards, so we reverse [min, max]
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
    const max = this.height - this.heightMargin;
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
  get containerSelection() {
    return d3.select('app-simple-line-graph');
  }

  get axisYSelection() {
    return this.svgSelection.select('.y-axis');
  }

  get svgSelection() {
    return d3.select(this.svg.nativeElement);
  }

  get theToolTip() {
    return d3.select('chef-tooltip');
  }

  get theRectHighlight() {
    return d3.select('.rect-highlight');
  }

  // returns a function that when passed our data, will return an svg path
  get createPath() {
    return d3.line()
              .x(d => this.xScale( d.daysAgo) )
              .y(d => this.yScale( d.percentage) );
  }

  get viewBox() {
    return `0 0 ${this.width} ${this.height}`;
  }

  createTheRingHighlight(): void {
      const highlight = this.svgSelection.selectAll('.ring-highlight').data([this.data]);
      highlight.exit().remove();
      highlight.enter().append('circle').attr('class', 'ring-highlight')
        .merge(highlight).attr('r', 10);

      this.theHighlight = d3.select('.ring-highlight');
  }

    ////////////////// RENDER FUNCTIONS ////////////////////
  renderChart() {
    this.AllUnlockDeactivate();
    this.resizeChart();
    this.createTheRingHighlight();
    this.renderGrid();
    this.renderLine();
    this.renderPoints();
  }

  renderLine(): void {
    // create the line using path function
    const line = this.createPath(this.data);

    const theLine = this.svgSelection.selectAll('.line').data([this.data]);
    theLine.exit().remove();
    theLine.enter().append('path').attr('class', 'line').merge(theLine)
    .transition().duration(1000)
    .attr('d', line);
  }

  renderPoints(): void {
    const points = this.svgSelection.selectAll('circle.point').data(this.data);
    points.exit().remove();
    points.enter().append('circle')
        .attr('class', (_d,i) => `circle point highlight-${i}`)
        .merge(points)
        .transition().duration(1000)
        .attr('percent', ( d => d.percentage ) ) // must add this data AFTER the merge
        .attr('cx', d => this.xScale(d.daysAgo))
        .attr('cy', d => this.yScale(d.percentage))
        .attr('r', 4);
  }


  renderGrid() {
    // create the X axis grid lines
    const xGrid = d3.axisTop()
      .ticks(this.data.length)
      .tickFormat('')
      .tickSize(this.height - ( this.heightMargin * ( 3 / 2 ) ) + 10) // magic numbers?
      .tickSizeOuter(0)
      .scale(this.xScale);
    // Render the X axis and X ticks
    const grid = this.svgSelection.selectAll('.grid').data([this.data]);
    grid.exit().remove();
    grid.enter().append('g').attr('class', 'grid')
      // this line will need to be updated to flexible
      .attr('transform', `translate(0, ${this.height - this.heightMargin})`)
      .merge(grid).transition().duration(1000)
      .call(xGrid);

    // create the Y axis
    const yAxis = d3.axisRight(this.yScale).tickFormat(d => d + '%').ticks(1);
    // render the Y axis
    const y = this.svgSelection.selectAll('.y-axis').data([this.data]);
    y.exit().remove();
    y.enter().append('g').attr('class', 'y-axis').merge(y)
      .transition().duration(1000)
      .call(yAxis);


    // tick Labels is responsible for converting the data to human readable
    const tickLabels = this.xData.map(data => {
      const daysPassed = data + 1;
      switch (daysPassed) {
        case 1:
          return '24 hrs ago';
          break;
        default:
          return `${daysPassed} days ago`;
      }
    }).reverse(); // because we reversed our data above, we need to reverse our labels as well
    // might be able to flip this back when real data is piping in

    const xAxis = d3.axisBottom().ticks(this.data.length)
      .tickSizeInner(10).tickSizeOuter(0).tickFormat((_d, i) => tickLabels[i])
      .scale(this.xScale);

    const x = this.svgSelection.selectAll('.x-axis').data([this.data]);
    x.exit().remove();
    x.enter().append('g').attr('class', 'x-axis')
      .attr('transform', `translate(0, ${this.height - this.heightMargin})`)
      .merge(x).transition().duration(1000)
      .call(xAxis);

    // remove zero from bottom of chart on x axis
    this.svgSelection.selectAll('.tick')
      .filter(tick => tick === 0)
      .remove();

    let tickLength = tickLabels.length; // make variable to reverse class assignment
    this.svgSelection.selectAll('.x-axis .tick text')
              // with real data we should be able to get rid of this and just use index
      .attr('class', () => `label-text highlight-${(tickLength--) - 1}`)
      .classed('turnt', () => {
        return tickLabels.length > 7;
      }).transition().duration(1000);

    this.svgSelection.selectAll('.label-text')
      .on('mouseenter', () => {
          if ( this.locked === true ) { return; }
          this.attachTooltipAndRing(d3.event);
          // don't show label gradient if too many labels
          if (this.data.length < 8) {
            this.createGradientLabel(d3.event);
          }
      })
      .on('mouseout', () => {
        if (this.locked === true) { return; }
        this.AllDeactivate();
      })
      .on('click', () => {
        if ( this.locked ) {
          // if locked, check if the target matches the highlighted number
          if ( !d3.event.target.classList
              .contains(`highlight-${this.currentHighlightnum}`) ) { return; }
          this.theToolTip.classed('active', true);
          this.allUnlock();
        } else {
          this.lockCircleGradient(d3.event);
          this.theToolTip.classed('active', false);
          this.theHighlight.classed('lock', true);
          // don't show labels gradient if too many labels
          if (this.data.length < 8) {
            this.theRectHighlight.classed('lock', true);
          }
          this.locked = true;
        }
      });
  }

  toggleLock(): void {
    this.locked = !this.locked;

  }

  createGradientLabel(d3Event): void {
    const containerCoords = this.containerSelection.node().getBoundingClientRect();
    // For the rectangle ring highlighter
    // Find the class to match with label
    const highlightNum = this.getClassHighlightNumber(d3Event);
    // get the text content from the label
    const text = d3.select(`.label-text.highlight-${highlightNum}`).text();
    // generate the box highlight ring size
    const textBounds = d3
      .select(`.label-text.highlight-${highlightNum}`).node().getBoundingClientRect();

    const rectWidth = textBounds.width + 16;
    const posLeft = (textBounds.x - containerCoords.x) - 8;
    const posBottom = (containerCoords.bottom - textBounds.bottom) - 3;

    // apply the highlight styles
    this.theRectHighlight.select('.rect-inner').text(() => text);
    this.theRectHighlight
      .style('left', `${posLeft}px`)
      .style('bottom', `${posBottom}px`)
      .style('width', `${rectWidth}px`)
      .classed('active', true);
  }

  attachTooltipAndRing(d3Event): void {
    // Find the class to match with label
    const highlightNum = this.getClassHighlightNumber(d3Event);

    // get the text content from the label
    const coords = d3.select(`.circle.highlight-${highlightNum}`).node().getBoundingClientRect();
    const highlightCircle = d3.select(`.circle.highlight-${highlightNum}`);
    const percentage = highlightCircle.attr('percent');

    // theToolTip is the tooltip that shows on hover
    this.theToolTip
      .style('left', `${ coords.left + 4 }px`)
    // 15px for thhe tooltip to breathe;
      .style('top', `${coords.top - 15}px`)
      .classed('active', true)
      .text(_d => `Checked-in ${ percentage }%`);

    // ring highlight position can come straight from the highlighted circle
    const cx = highlightCircle.attr('cx');
    const cy = highlightCircle.attr('cy');
    this.theHighlight
      .attr('cx', cx)
      .attr('cy', cy)
      .classed('active', true);
  }

  lockCircleGradient(d3Event): void {
    const highlightNum = this.getClassHighlightNumber(d3Event);
    d3.select(`.circle.highlight-${highlightNum}`).classed('lock', 'true');
  }

  getClassHighlightNumber(d3Event): number {
    const classes = d3.select(d3Event.target).attr('class');
    const match = classes.match(/highlight-([0-9]{1,2})/g)[0];
    const num = match.split('-')[1];
    this.currentHighlightnum = num;
    return num;
  }

  AllDeactivate(): void {
    d3.selectAll('.active').classed('active', false); // deactivate any active items on page
  }

  allUnlock(): void {
    d3.selectAll('.lock').classed('lock', false); // unlock any locked items on page
    this.locked = false;
  }

  AllUnlockDeactivate(): void {
    this.allUnlock();
    this.AllDeactivate(); // deactivate any active items on page
  }


  ngOnChanges() {
    this.renderChart();
  }

  onResize(): void {
    this.renderChart();
  }

  resizeChart(): void {
    this.width = this.chart.nativeElement.getBoundingClientRect().width;
  }

}

