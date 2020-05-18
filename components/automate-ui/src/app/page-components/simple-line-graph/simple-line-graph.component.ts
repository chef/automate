import {
  Component, Input, OnChanges, ViewChild, ElementRef, SimpleChanges, OnInit
} from '@angular/core';
import * as d3 from 'd3';
import {
  DayPercentage
} from 'app/entities/desktop/desktop.model';


@Component({
  selector: 'app-simple-line-graph',
  templateUrl: './simple-line-graph.component.html',
  styleUrls: ['./simple-line-graph.component.scss']
})

export class SimpleLineGraphComponent implements OnChanges, OnInit {

  // labels too high
  // disappearing render

  constructor(
    private chart: ElementRef
  ) { }

  @ViewChild('svg', { static: true }) svg: ElementRef;
  @Input() data: DayPercentage[] = [];
  @Input() width = 900;
  @Input() height = 156; // we want a 116px height on the ticks, this minus margins
  private locked: number = null;
  private t = 1000; // transition speed

  get margin() {
    return { right: 20, left: 20, top: 20, bottom: 20 };
  }
  ////////   X AXIS ITEMS   ////////
  // maps all of our x data points
  get xData(): number[] {
    return this.data.map(d => d.daysAgo);
  }
  // determines how wide the graph should be to hold our data
  // in its respective area;
  get rangeX(): [number, number] {
    const min = this.margin.left;
    const max = this.width - this.margin.right;
    return [max, min];  // we want to plot our data backwards, so we reverse [min, max]
  }
  // determines the min and max values of the x axis
  get domainX(): [number, number] {
    const min = Math.min(...this.xData);
    const max = Math.max(...this.xData);
    return [min, max];
  }

  get xScale() {
    return d3.scaleTime()
      .range(this.rangeX)
      .domain(this.domainX);
  }


  ////////   Y AXIS ITEMS   ////////
  get rangeY(): [number, number] {
    const min = this.margin.top;
    const max = this.height - this.margin.bottom;
    return [max, min];
  }

  get domainY(): [number, number] {
    const min = 0;
    const max = 100; // since this based on a percentage we are doing 0 to 100;
    return [min, max];
  }

  get yScale() {
    return d3.scaleLinear()
      .range(this.rangeY)
      .domain(this.domainY);
  }

  //////// SELECTIONS ////////
  get containerSelection() {
    return d3.select('app-simple-line-graph');
  }

  get labelContainerSelection() {
    return d3.select('.label-container');
  }

  get svgSelection() {
    return d3.select(this.svg.nativeElement);
  }

  get axisYSelection() {
    return this.svgSelection.select('.y-axis');
  }

  // returns a function that when passed our data, will return an svg path
  get createPath() {
    return d3.line()
      .x(d => this.xScale(d.daysAgo))
      .y(d => this.yScale(d.percentage));
  }

  get viewBox() {
    return `0 0 ${this.width} ${this.height}`;
  }

  ////////////////// RENDER FUNCTIONS ////////////////////
  private renderChart() {
    this.AllUnlockDeactivate();
    this.resizeChart();
    this.renderGrid();
    this.renderLine();
    this.renderPoints();
    this.renderTooltips();
    this.renderRings();
    this.renderLabelButtons();
    this.relock();
    this.t = 1000;  // reset the standard transition speed to 1000 since it could be zero
  }

  private renderLine(): void {
    // create the line using path function
    const line = this.createPath(this.data);

    const theLine = this.svgSelection.selectAll('.line').data([this.data], d => d.daysAgo);
    theLine.exit().remove();
    theLine.enter().append('path').attr('class', 'line').merge(theLine)
      .transition().duration(this.t)
      .attr('d', line);
  }

  private renderPoints(): void {
    const points = this.svgSelection.selectAll('circle.point')
      .data(this.data, d => d.daysAgo);
    points.exit().remove();
    points.enter().append('circle')
      .attr('class', (_d, i) => `point elem-${i}`)
      .merge(points)
      .transition().duration(this.t)
      .attr('percent', (d => d.percentage))
      .attr('cx', d => this.xScale(d.daysAgo))
      .attr('cy', d => this.yScale(d.percentage))
      .attr('r', 4);
  }

  private renderRings(): void {
    const rings = this.svgSelection.selectAll('circle.ring')
      .data(this.data, d => d.daysAgo);
    rings.exit().remove();
    rings.enter().append('circle')
      .attr('class', (_d, i) => `ring elem-${i}`)
      .merge(rings)
      .transition().duration(this.t)
      .attr('cx', d => this.xScale(d.daysAgo))
      .attr('cy', d => this.yScale(d.percentage))
      .attr('r', 10);
  }

  private renderTooltips(): void {
    // these numbers are specific to its container
    const localWidth = this.width - this.margin.right - this.margin.left - 34;
    const thisRange = [localWidth, -this.margin.right];
    const thisScale = d3.scaleLinear()
      .domain(this.domainX)
      .range(thisRange);

    const tooltips = this.containerSelection.selectAll('div.graph-tooltip')
      .data(this.data, d => d.daysAgo);
    tooltips.exit().remove();
    tooltips.enter().append('div')
      .attr('class', (_d, i: number) => `graph-tooltip elem-${i}`)
      .merge(tooltips)
      .text(d => `Checked in ${Math.round(d.percentage)}%`)
      .style('left', (d, i: number) => {
        const left = thisScale(d.daysAgo);
        if (i === 0) { return `${localWidth + this.margin.right}px`; }
        return `${left}px`;
      })
      .style('top', d => `${this.yScale(d.percentage) - 10}px`);
  }

  private renderLabelButtons(): void {
    // these numbers are specific to its container
    const thisRange = [this.width - 48, 53];
    const thisScale = d3.scaleLinear()
      .domain(this.domainX)
      .range(thisRange);

    const labels = this.labelContainerSelection.selectAll('.graph-button')
      .data(this.data, d => d.daysAgo);
    labels.exit().remove();
    labels.enter().append('button')
      .call(parent => {
        parent.append('div')
          .attr('class', (_d, i: number) => `inner elem-${i}`);
      })
      .attr('class', (_d, i: number) => `graph-button elem-${i}`)
      .merge(labels)
      .transition().duration(this.t)
      .style('left', d => {
        if (this.xData.length > 7) {
          return `${this.xScale(d.daysAgo) - 30}px`;
        } else {
          return `${thisScale(d.daysAgo) - 30}px`;
        }
      })
      .call(parent => {
        parent.select('.inner')
          .text(p => this.formatLabels(p.daysAgo));
      });

    // add all listeners
    this.containerSelection.selectAll('.graph-button')
      // add class to rotate labels when more than comfortable to fit in space
      .classed('turnt', () => this.xData.length > 7)

      .on('mouseenter', () => {
        this.handleHover(d3.event);
      })
      .on('mouseout', () => {
        this.AllDeactivate();
      })
      // focus styles
      .on('focus', () => {
        this.handleHover(d3.event);
      })
      .on('focusout', () => {
        this.AllDeactivate();
      })
      .on('click', () => {
        this.handleClick(d3.event);
      });
  }

  private formatLabels(daysAgo: number): string {
    switch (daysAgo) {
      case 0:
        return '24 hrs ago';
        break;
      default:
        return `${daysAgo + 1} days ago`;
    }
  }

  private renderGrid() {
    // create the X axis grid lines
    const xGrid = d3.axisTop()
      .ticks(this.data.length)
      .tickFormat('')
      .tickSize(this.height - (this.margin.bottom + this.margin.top))
      .tickSizeOuter(0)
      .scale(this.xScale);
    // Render the X grid lines
    const grid = this.svgSelection.selectAll('.grid').data([this.data]);
    grid.exit().remove();
    grid.enter().append('g').attr('class', 'grid')
      .attr('transform', `translate(0, ${this.height - this.margin.bottom})`)
      .merge(grid).transition().duration(this.t)
      .call(xGrid);

    // create the Y axis
    const yAxis = d3.axisRight(this.yScale).tickFormat(d => d + '%').ticks(1);
    // render the Y axis
    const y = this.svgSelection.selectAll('.y-axis').data([this.data]);
    y.exit().remove();
    y.enter().append('g').attr('class', 'y-axis')
      .attr('transform', `translate(${this.margin.left}, 0)`)
      .merge(y).transition().duration(this.t)
      .call(yAxis);

    // create the X axis
    const xAxis = d3.axisBottom().ticks(this.data.length)
      .tickSizeInner(10).tickSizeOuter(0).tickFormat('')
      .scale(this.xScale);
    // render the X axis
    const x = this.svgSelection.selectAll('.x-axis').data([this.data]);
    x.exit().remove();
    x.enter().append('g').attr('class', 'x-axis')
      .attr('transform', `translate(0, ${this.height - this.margin.bottom})`)
      .merge(x).transition().duration(this.t)
      .call(xAxis);

    // remove zero from bottom of chart on X axis
    this.svgSelection.selectAll('.tick')
      .filter(tick => tick === 0)
      .remove();
  }

  private handleHover(d3Event): void {
    const num = this.getHoveredElement(d3Event);
    d3.selectAll(`.elem-${num}`).classed('active', true);
  }

  private handleClick(d3Event): void {
    const num = this.getHoveredElement(d3Event);
    const isAlreadyLocked = d3.selectAll(`.elem-${num}`).classed('lock');
    if (isAlreadyLocked) {
      d3.selectAll(`.elem-${num}`).classed('lock', false);
      this.locked = null;
    } else {
      d3.selectAll('.lock').classed('lock', false);
      d3.selectAll(`.elem-${num}`).classed('lock', true);
      this.locked = num;
    }
  }

  private getHoveredElement(d3Event): number {
    const classes = d3.select(d3Event.target).attr('class');
    const match = classes.match(/elem-([0-9]{1,2})/g)[0];
    const num = match.split('-')[1];
    return num;
  }

  private relock(): void {
    if (this.locked) {
      d3.selectAll(`.elem-${this.locked}`).classed('lock', true);
    }
  }

  private AllDeactivate(): void {
    d3.selectAll('.active').classed('active', false);
  }

  private allUnlock(): void {
    d3.selectAll('.lock').classed('lock', false);
  }

  private AllUnlockDeactivate(): void {
    this.allUnlock();
    this.AllDeactivate();
  }

  onResize(): void {
    this.t = 0; // turn off transitions for resize events;
    this.renderChart();
  }

  private resizeChart(): void {
    this.width = this.chart.nativeElement.getBoundingClientRect().width;
  }

  ngOnInit() {
    if (this.data && this.data.length > 0) {
      this.renderChart();
    }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.data && !changes.data.firstChange) {
      this.renderChart();
    }
  }

}

