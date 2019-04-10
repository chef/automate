import { Component, OnChanges, SimpleChange, Input, AfterContentInit,
  ChangeDetectionStrategy } from '@angular/core';
import {
  NodeDetailsService
} from '../../services/node-details/node-details.service';
import * as moment from 'moment';
import { NodeRun } from '../../types/types';

@Component({
  selector: 'app-run-summary',
  templateUrl: './run-summary.component.html',
  styleUrls: ['./run-summary.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class RunSummaryComponent implements OnChanges, AfterContentInit {
  @Input() nodeRun: NodeRun;
  public percentComplete: number;

  private convergeResources: any[];
  public chartSucceeded;
  public chartFailed;
  public chartOther;

  constructor(
    private eventService: NodeDetailsService
  ) { }

  ngAfterContentInit() {
    this.loadConverge(this.nodeRun);
  }

  ngOnChanges(changes: {[propertyName: string]: SimpleChange}) {
    if (changes['nodeRun']) {
      this.nodeRun = changes['nodeRun'].currentValue;
    }
    this.loadConverge(this.nodeRun);
  }

  loadConverge(nodeRun: NodeRun) {
    this.convergeResources = nodeRun.resources;
    this.updateRadial();
  }

  renderDuration(totalSeconds) {
    return moment.duration(totalSeconds, 'seconds').humanize();
  }

  renderTime(timestamp) {
    return moment(timestamp).format('LT');
  }

  renderDate(timestamp) {
    return moment(timestamp).format('L');
  }

  nodeRunDeprecations(deprecations) {
    if (deprecations) {
      return deprecations.length;
    } else {
      return 0;
    }
  }

  activateModal() {
    this.eventService.showModal(true);
  }

  private setCompleteness(): void {
    let completed = 0, not = 0, percentage = 0;

    if (this.convergeResources) {

      if (this.convergeResources.length === 0 &&
        this.nodeRun.status === 'success') {

        percentage = 100;

      } else {

        this.convergeResources.forEach(r => {
          switch (r.status) {
            case 'up-to-date':
            case 'skipped':
            case 'updated':
              completed++;
              break;
            case 'unprocessed':
              not++;
              break;
            case 'failed':
              if (r.ignore_failure) {
                completed++;
              } else {
                not++;
              }
              break;
          }
        });

        const total = completed + not;

        if (total !== 0) {
          percentage = Math.ceil(completed / (completed + not) * 100);
        }
      }
    }

    this.percentComplete = percentage;
  }

  private updateRadial(): void {
    this.setCompleteness();
    this.chartSucceeded = this.percentComplete;
    this.chartOther = 99 - this.percentComplete;

    if (this.percentComplete < 100) {
      // This might look a little odd, but it lets us indicate failure visually
      // (with a thin red arc on the chart) without having to resort to lower-level
      // Chart.js manipulation; when a converge fails, we just insert a "1%" marker
      // for it.
      this.chartFailed = 1;
    } else {
      this.chartFailed = 0;
    }
  }
}
