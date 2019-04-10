import { Component, Inject, Input, OnChanges, OnDestroy, ViewChild } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import * as d3 from 'd3';

let UID = 0;

@Component({
  selector: 'app-overview-failures',
  templateUrl: './overview-failures.component.html',
  styleUrls: ['./overview-failures.component.scss']
})
export class OverviewFailuresComponent implements OnChanges, OnDestroy {

  constructor(
    @Inject(DOCUMENT) private document: Document
  ) {
    this.id = `failures-${UID++}`;
  }

  @Input() data = [];

  @Input() vbWidth = 400;

  @Input() vbHeight = 300;

  @ViewChild('svg') svg;

  id: string;

  get viewBox() {
    return `0 0 ${this.vbWidth} ${this.vbHeight}`;
  }

  get bubbleData() {
    if (!this.data.length) { return []; }

    const root = d3.hierarchy({ children: this.data })
      .sum(d => d.failures);

    const pack = d3.pack()
      .size([this.vbWidth, this.vbHeight])
      .padding(4);

    return pack(root).leaves();
  }

  get bubbleSelection() {
    return d3.select(this.svg.nativeElement).selectAll('.bubble')
      .data(this.bubbleData);
  }

  get tipsSelection() {
    return d3.select(this.document.body).selectAll(`.bubble-tip-${this.id}`)
      .data(this.bubbleData);
  }

  ngOnChanges() {
    this.draw();
  }

  ngOnDestroy() {
    this.clear();
  }

  clear() {
    this.bubbleSelection.remove();
    this.tipsSelection.remove();
  }

  draw() {
    this.drawBubbles();
    this.drawTips();
  }

  drawBubbles() {
    const enter = this.bubbleSelection.enter().append('g');
    const update = this.bubbleSelection.merge(enter);
    const exit = this.bubbleSelection.exit();

    const mouseover = (_d, i, ns) => {
      d3.selectAll(ns.filter(n => n !== ns[i])).transition()
        .style('opacity', 0.5);
    };
    const mouseout = (_d, _i, ns) => {
      d3.selectAll(ns).transition()
        .style('opacity', 1);
    };

    enter
      .attr('class', 'bubble');
    enter.append('circle');
    enter.append('text')
      .attr('text-anchor', 'middle')
      .attr('y', '3');

    update
      .attr('transform', d => `translate(${d.x},${d.y})`);
    update.select('circle')
      .attr('id', (_d, i) => `bubble-${this.id}-${i}`)
      .attr('r', d => d.r)
      .on('mouseover', mouseover)
      .on('mouseout', mouseout);
    update.select('text')
      .text(d => d.data.name.substring(0, d.r / 4));

    exit.remove();
  }

  drawTips() {
    const enter = this.tipsSelection.enter().append('chef-tooltip');
    const update = this.tipsSelection.merge(enter);
    const exit = this.tipsSelection.exit();

    enter
      .attr('class', `bubble-tip bubble-tip-${this.id}`)
      .attr('follow', true);
    enter.append('p')
      .attr('class', 'bubble-tip-name');
    enter.append('p')
      .attr('class', 'bubble-tip-text');

    update
      .attr('for', (_d, i) => `bubble-${this.id}-${i}`);
    update.select('.bubble-tip-name')
      .text(d => d.data.name);
    update.select('.bubble-tip-text')
      .text(d => `${d3.format(',')(d.data.failures)} failed nodes`);

    exit.remove();
  }
}
