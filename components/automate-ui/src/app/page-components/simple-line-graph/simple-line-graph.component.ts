import {
  Component, Input, OnChanges, ViewChild, ElementRef, OnInit
} from '@angular/core';
import * as d3 from 'd3';


@Component({
  selector: 'app-simple-line-graph',
  templateUrl: './simple-line-graph.component.html',
  styleUrls: ['./simple-line-graph.component.scss']
})

export class SimpleLineGraphComponent implements OnChanges, OnInit {

  constructor(
    private chart: ElementRef
  ) {}

  @Input() data: any = [];
  @Input() width = 900;
  @Input() height = 200;

  @ViewChild('svg', {static: true}) svg: ElementRef;

  heightMargin = 40;

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
  get axisYSelection() {
    return this.svgSelection.select('.y-axis');
  }

  get svgSelection() {
    return d3.select(this.svg.nativeElement);
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

  resizeChart() {
    this.width = this.chart.nativeElement.getBoundingClientRect().width;
  }

    ////////////////// RENDER FUNCTIONS ////////////////////
  renderChart() {
    this.resizeChart();
    this.renderGrid();
    this.renderLine();
    this.renderPoints();
  }

  renderLine() {
    // create the line using path function
    const line = this.createPath(this.data);

    const theLine = this.svgSelection.selectAll('.line').data([this.data]);
    theLine.exit().remove();
    theLine.enter().append('path').attr('class', 'line').merge(theLine)
    .transition().duration(1000)
    .attr('d', line);
  }

  renderPoints() {
    // render tooltip
    const toolTip = d3.select('body').selectAll('chef-tooltip').data([this.data]);
    toolTip.exit().remove();
    toolTip.enter().append('chef-tooltip')
      .attr('for', 'tooltip').merge(toolTip);

    // render highlight ring
    const highlight = d3.select('body').selectAll('.highlight').data([this.data]);
    highlight.exit().remove();
    highlight.enter().append('div').attr('class', 'highlight').merge(highlight);

    // Create selections for the tooltip and highlight
    const theToolTip = d3.select('chef-tooltip');
    const theHighlight = d3.select('.highlight');
    const rectHighlight = d3.select('body')
      .append('div').attr('class', 'rect-highlight');
      // for linear gradient, we attach another div;
      rectHighlight.append('div').attr('class', 'rect-inner');

    const points = this.svgSelection.selectAll('circle').data(this.data);
    points.exit().remove();
    points
      .enter()
      .append('circle')
        .on('mouseover', (cd) => {
          console.log(d3.event);
          const theTarget = d3.select(d3.event.target);
          theTarget.classed('active', 'true');

          // highlight is the ring around the point on hover
          const highlightPos = d3.event.target.getBoundingClientRect();

          theHighlight
            .style('left', `${(highlightPos.x - 8)}px`) // 8 is half highlight width
            .style('top', `${(highlightPos.y - 8)}px`) // minus diameter of point
            .classed('active', true);

          // theToolTip is the tooltip that shows on hover
          theToolTip.style('left', `${d3.event.pageX}px`);
          theToolTip.style('top', `${d3.event.pageY - 15}px`); // 15px for thhe tooltip to breathe;
          theToolTip.classed('active', true);
          theToolTip.text(_d => `Checked-in ${cd.percentage}%`);


          // For the rectangle ring highlighter
          // Find the class to match with label
          const classes = theTarget.attr('class');
          const match = classes.match(/circle-([0-9]{1,2})/g)[0];
          const num = match.split('-')[1];


          const text = d3.select(`.label-text-${num}`).text();
          console.log(text);

          const textBounds = d3.select(`.label-text-${num}`).node().getBoundingClientRect();
          const rectWidth = textBounds.width + 16;
          const textLeft = textBounds.x - ( rectWidth / 2)  + (textBounds.width / 2);
          const textTop = textBounds.y - ( textBounds.height / 2 ) + 2;

          rectHighlight.select('.rect-inner').text(() => text);
          rectHighlight.style('left', `${textLeft}px`);
          rectHighlight.style('top', `${textTop}px`);
          rectHighlight.style('width', `${rectWidth}px`);
          rectHighlight.classed('active', true);
        })
        .on('mouseout', () => {
          d3.select(d3.event.target).classed('active', false);
          theToolTip.classed('active', false);
          theHighlight.classed('active', false);
          rectHighlight.classed('active', false);
          })
        .attr('class', 'circle')
        .attr('class', (_d,i) => `circle-${i}`)
        .merge(points)
        .transition().duration(1000)
        .attr('cx', d => this.xScale(d.daysAgo))
        .attr('cy', d => this.yScale(d.percentage))
        .attr('r', 4);
  }


  renderGrid() {
    // create the X axis grid lines
    const xGrid = d3.axisTop()
      .ticks(this.data.length)
      .tickFormat('')
      .tickSize(this.height - ( this.heightMargin * ( 3 / 2 ) ))
      .tickSizeOuter(0)
      .scale(this.xScale);
    // Render the X axis and X ticks
    const grid = this.svgSelection.selectAll('.grid').data([this.data]);
    grid.exit().remove();
    grid.enter().append('g').attr('class', 'grid')
      // this line will need to be updated to flexible
      .attr('transform', `translate(0, ${this.height - this.heightMargin })`)
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

    const xAxis = d3.axisBottom().ticks(this.data.length)
      .tickSizeInner(10).tickSizeOuter(0).tickFormat((_d, i) => tickLabels[i])
      .scale(this.xScale);

    const x = this.svgSelection.selectAll('.x-axis').data([this.data]);
    x.exit().remove();
    x.enter().append('g').attr('class', 'x-axis')
      .attr('transform', `translate(0, ${this.height - this.heightMargin })`)
      .merge(x).transition().duration(1000)
      .call(xAxis);

    // remove zero from bottom of chart on x axis
    this.svgSelection.selectAll('.tick')
      .filter(tick => tick === 0)
      .remove();

    let tickLength = tickLabels.length; // make variable to reverse class assignment
    this.svgSelection.selectAll('.x-axis .tick text')
      .attr('class', () => `label-text label-text-${(tickLength--) - 1}`)
      .classed('turnt', () => {
        return tickLabels.length > 7;
      }).transition().duration(1000);
  }


  ngOnChanges() {
    console.log('change');
    this.renderChart();
  }

  ngOnInit() {
    // console.log(this.data);
  }

  onResize() {
    this.renderChart();
  }
}

