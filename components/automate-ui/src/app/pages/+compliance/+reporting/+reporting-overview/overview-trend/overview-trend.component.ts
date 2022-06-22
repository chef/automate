import {
  Component,
  ElementRef,
  EventEmitter,
  Inject,
  Input,
  Output,
  OnChanges,
  OnDestroy,
  ViewChild,
  OnInit
} from '@angular/core';
import { DOCUMENT } from '@angular/common';
import * as d3 from 'd3';
import * as moment from 'moment/moment';
import { DateTime } from 'app/helpers/datetime/datetime';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';
import { Subscription } from 'rxjs';

export interface TrendData {
  report_time: Date;
  failed: number;
  passed: number;
  skipped: number;
  waived: number;
}

@Component({
  selector: 'app-overview-trend',
  templateUrl: './overview-trend.component.html',
  styleUrls: ['./overview-trend.component.scss']
})
export class OverviewTrendComponent implements OnChanges, OnDestroy, OnInit  {

  constructor(
    private el: ElementRef,
    @Inject(DOCUMENT) private document: Document,
    private userPrefsService: UserPreferencesService
  ) {}

  @Input() data = [];

  @Output() dateSelected: EventEmitter<string> = new EventEmitter<string>();

  @Input() type: 'nodes' | 'controls';

  @Input() vbWidth = 900;

  @Input() vbHeight = 300;

  @ViewChild('svg', { static: true }) svg;

  trendDataCache = [];
  private timeFormatValue: string = DateTime.CHEF_DATE_TIME;
  private userPreferenceSubscription: Subscription;

  get viewBox() {
    return `0 0 ${this.vbWidth} ${this.vbHeight}`;
  }

  private get trendData(): TrendData[] {
    if (this.trendDataCache.length === 0) {
      this.trendDataCache = this.data.map(d => {
        return { ...d, report_time: this.createUtcDate(d.report_time) };
      });
    }
    return this.trendDataCache;
  }

  private get domainX() {
    const min = d3.min(this.trendData, (d: TrendData) => d.report_time);
    const max = d3.max(this.trendData, (d: TrendData) => d.report_time);
    return [min, max];
  }

  private get rangeX() {
    const min = 50;
    const max = this.vbWidth - 50;
    return [min, max];
  }

  private get scaleX() {
    return d3.scaleTime()
      .domain(this.domainX)
      .range(this.rangeX);
  }

  private get domainY() {
    const min = 0;
    const max = d3.max(this.trendData, (d: TrendData) => d3.max([
      d.failed,
      d.passed,
      d.skipped,
      d.waived
    ]));
    return [min, max];
  }

  private get rangeY() {
    const min = 10;
    const max = this.vbHeight - 30;
    return [max, min];
  }

  private get scaleY() {
    return d3.scaleLinear()
      .domain(this.domainY)
      .range(this.rangeY);
  }

  private get svgSelection() {
    return d3.select(this.svg.nativeElement);
  }

  private get axisXSelection() {
    return this.svgSelection.select('.x-axis');
  }

  private get axisYSelection() {
    return this.svgSelection.select('.y-axis');
  }

  private get dotsSelection() {
    return this.svgSelection.selectAll('.dot-group')
      .data(this.trendData, (d: TrendData) => d.report_time.getTime());
  }

  private get tipsSelection() {
    return d3.select(this.document.body).selectAll('.dot-group-tip')
      .data(this.trendData, (d: TrendData) => d.report_time.getTime());
  }

  private get linesSelection() {
    return this.svgSelection.selectAll('.status-line')
      .data([
        'skipped',
        'passed',
        'failed',
        'waived'
      ].map(status => ({ status, values: this.trendData })));
  }
  ngOnChanges() {
    this.trendDataCache = [];
    this.resize();
    this.draw();
  }

  ngOnInit() {
    this.userPreferenceSubscription = this.userPrefsService.timeformat$.subscribe((timeFormat) => {
      this.timeFormatValue = timeFormat.value;
    });
  }

  ngOnDestroy() {
    this.clear();
    if (this.userPreferenceSubscription) {
      this.userPreferenceSubscription.unsubscribe();
    }
  }

  onResize() {
    this.resize();
    this.draw();
  }

  clear() {
    this.svgSelection.remove();
    this.tipsSelection.remove();
  }

  resize() {
    this.vbWidth = this.el.nativeElement.getBoundingClientRect().width;
  }

  draw() {
    this.drawAxisX();
    this.drawAxisY();
    this.drawLines();
    this.drawDots();
    this.drawTips();
  }

  drawAxisX() {
    this.axisXSelection.select('text')
      .attr('x', this.vbWidth / 2)
      .attr('y', this.vbHeight - 3)
      .text(() => {
        return [
          moment.utc(this.domainX[0]).format(this.timeFormatValue),
          moment.utc(this.domainX[1]).format(this.timeFormatValue)
        ].join(' - ');
      });

    this.axisXSelection.select('.domain')
      .attr('x1', this.rangeX[0])
      .attr('y1', this.rangeY[0])
      .attr('x2', this.rangeX[1])
      .attr('y2', this.rangeY[0]);
  }

  drawAxisY() {
    const tickFormat = d => {
      if (d >= 1000) { return d3.format('4.3s')(d); }
      return Math.floor(d) === d ? d : '';
    };
    const drawAxis = d3.axisLeft(this.scaleY)
      .ticks(10)
      .tickFormat(tickFormat);
    this.axisYSelection.call(drawAxis);
  }

  drawLines() {
    const linesEnter = this.linesSelection.enter().append('path');
    const linesUpdate = this.linesSelection.merge(linesEnter);
    const linesExit = this.linesSelection.exit();

    const drawLine = status => d3.line()
      .x(d => this.scaleX(d.report_time))
      .y(d => this.scaleY(d[status]))
      .curve(d3.curveMonotoneX);

    linesUpdate
      .attr('class', d => `status-line ${d.status}`)
      .attr('d', d => drawLine(d.status)(d.values));

    linesExit.remove();
  }

  drawDots() {
    const enter = this.dotsSelection.enter().append('g');
    const update = this.dotsSelection.merge(enter);
    const exit = this.dotsSelection.exit();

    const maxDots = 10;
    const statuses = ['failed', 'passed', 'skipped', 'waived'];
    const isHidden = (_d, i, ns) => i % Math.round(ns.length / maxDots) !== 0;

    enter
      .attr('class', 'dot-group');
    enter.append('line')
      .attr('class', 'dot-group-tick');

    statuses.forEach(status => {
      enter.append('circle')
        .attr('class', `status-dot ${status}`)
        .attr('r', 4);
    });

    enter.append('rect')
      .attr('class', 'dot-group-bg');

    enter.on('click', (_d, i) => {
      this.dateSelected.next(moment(i.report_time).format('YYYY-MM-DD'));
    });

    update.select('.dot-group-tick')
      .attr('x1', d => this.scaleX(d.report_time))
      .attr('y1', this.rangeY[1])
      .attr('x2', d => this.scaleX(d.report_time))
      .attr('y2', this.rangeY[0])
      .classed('hidden', isHidden);

    statuses.forEach(status => {
      update.select(`.status-dot.${status}`)
        .attr('cy', d => this.scaleY(d[status]))
        .attr('cx', d => this.scaleX(d.report_time))
        .classed('hidden', isHidden);
    });

    update.select('.status-dot.failed')
      .attr('r', d => {
        if (d.failed === d.passed && d.failed === d.skipped && d.failed === d.waived) { return 10; }
        if (d.failed === d.passed && d.failed === d.skipped) { return 8; }
        if (d.failed === d.passed && d.failed === d.waived) { return 8; }
        if (d.failed === d.skipped && d.failed === d.waived) { return 8; }
        if (d.failed === d.passed || d.failed === d.skipped || d.failed === d.waived) { return 6; }
        return 4;
      });

    update.select('.status-dot.passed')
      .attr('r', d => {
        if (d.passed === d.skipped && d.passed === d.waived) { return 8; }
        if (d.passed === d.skipped || d.passed === d.waived) { return 6; }
        return 4;
      });

    update.select('.status-dot.skipped')
      .attr('r', d => d.skipped === d.waived ? 6 : 4);

    update.select('.dot-group-bg')
      .attr('id', (_d, i) => `dot-group-bg-${i}`)
      .attr('x', (d, _i, ns) => {
        const position = this.scaleX(d.report_time);
        const width = this.vbWidth / ns.length;
        const offset = width / 2;
        return position - offset;
      })
      .attr('y', this.rangeY[1])
      .attr('width', (_d, _i, ns) => this.vbWidth / ns.length)
      .attr('height', this.rangeY[0]);

    exit.remove();
  }

  drawTips() {
    const enter = this.tipsSelection.enter().append('chef-tooltip');
    const update = this.tipsSelection.merge(enter);
    const exit = this.tipsSelection.exit();

    const statuses = ['failed', 'passed', 'skipped', 'waived'];
    const tipText = (count, status, type) => `${d3.format(',')(count)} ${status} ${type}`;

    enter
      .attr('class', 'dot-group-tip')
      .attr('delay', '0');
    enter.append('p')
      .attr('class', 'tip-name');
    enter.append('div')
      .attr('class', 'tip-legend');

    statuses.forEach(status => {
      enter.select('.tip-legend').append('div')
        .attr('class', `tip-legend-item ${status}`);
    });

    update
      .attr('for', (_d, i) => `dot-group-bg-${i}`);
    update.select('.tip-name')
      .text(d => d3.timeFormat('%a %B %e %Y (UTC)')(d.report_time));

    statuses.forEach(status => {
      update.select(`.tip-legend-item.${status}`)
        .text(d => tipText(d[status], status, this.type));
    });

    exit.remove();
  }

  createUtcDate(time: string): Date {
    const utcDate = moment.utc(time);
    if (utcDate.isValid) {
      return new Date(utcDate.year(), utcDate.month(), utcDate.date());
    } else {
      console.error('Not a valid date ' + time);
      return new Date();
    }
  }
}
