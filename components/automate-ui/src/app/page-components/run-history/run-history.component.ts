import { Component, OnInit, OnDestroy, Input, Output, ChangeDetectionStrategy, ChangeDetectorRef,
  EventEmitter } from '@angular/core';
import {
  AbridgedNodeRun,
  NodeRunsCount,
  SelectedStatus,
  NodeHistoryFilter,
  NodeHistoryCountsFilter,
  RunInfo } from '../../types/types';
import { DateTime } from 'app/helpers/datetime/datetime';
import { NodeRunsService } from '../../services/node-details/node-runs.service';
import { HistorySelection } from '../../helpers/history-selection/history-selection';
import { RunHistoryStore } from '../../services/run-history-store/run-history.store';
import { Subscription } from 'rxjs';
import * as moment from 'moment/moment';
import { saveAs } from 'file-saver';
import {
  finalize
} from 'rxjs/operators';

@Component({
  selector: 'app-run-history',
  templateUrl: './run-history.component.html',
  styleUrls: ['./run-history.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class RunHistoryComponent implements OnInit, OnDestroy {

  @Input() nodeName: string;
  @Input() nodeId: string;
  @Input() initialRunId: string;
  @Input() initialDate: Date;
  @Input() visible: boolean;
  @Output() on_run_change: EventEmitter<RunInfo> = new EventEmitter<RunInfo>();
  @Output() closeRunHistoryEvent: EventEmitter<any> = new EventEmitter();

  nodeRunsCount: NodeRunsCount;
  nodeHistory: AbridgedNodeRun[];
  currentPage = 1;
  pageSize = 10;
  RFC2822 = DateTime.RFC2822;
  // store enum as member to be able to access it via html templates
  selectedStatus = SelectedStatus;
  // store selection
  selected: SelectedStatus = SelectedStatus.All;
  // selected item
  activeRunId: string;
  defaultSelectionTerm: string;
  private nodeHistoryFilterSubscription: Subscription;
  private nodeHistoryCountsFilterSubscription: Subscription;

  constructor(
    private changeDetectorRef: ChangeDetectorRef,
    private nodeHistoryStore: RunHistoryStore,
    private nodeRunsService: NodeRunsService
  ) { }

  ngOnInit() {
    this.activeRunId = this.initialRunId;
    this.nodeRunsCount = new NodeRunsCount({total: 0, success: 0, failure: 0});
    if (this.nodeId) {
      if (this.initialDate) {
        this.defaultSelectionTerm =
          HistorySelection.findIncludedDurationTerm(this.initialDate);
      } else {
        this.defaultSelectionTerm =
          HistorySelection.findIncludedDurationTerm(new Date());
      }

      this.nodeHistoryStore.nodeHistoryFilter = {
        startDate: this.formatDate(HistorySelection.startingTimestamp(this.defaultSelectionTerm)),
        nodeId: this.nodeId,
        page: 1,
        pageSize: this.pageSize
      };

      this.nodeHistoryStore.nodeHistoryCountsFilter = {
        startDate: this.formatDate(HistorySelection.startingTimestamp(this.defaultSelectionTerm)),
        nodeId: this.nodeId
      };
    }

    this.nodeHistoryFilterSubscription =
      this.nodeHistoryStore.filter.subscribe(nodeHistoryFilter => {
        this.loadHistory(nodeHistoryFilter);
      });

    this.nodeHistoryCountsFilterSubscription =
      this.nodeHistoryStore.countsFilter.subscribe(nodeHistoryCountsFilter => {
        this.loadRunCounts(nodeHistoryCountsFilter);
      });
  }

  ngOnDestroy() {
    if (this.nodeHistoryFilterSubscription) {
      this.nodeHistoryFilterSubscription.unsubscribe();
    }
    if (this.nodeHistoryCountsFilterSubscription) {
      this.nodeHistoryCountsFilterSubscription.unsubscribe();
    }
  }

  // returning US date format
  renderDate(datestamp) {
    return moment.utc(datestamp).format('MM/DD/YY');
  }

  renderTime(datestamp) {
    return moment.utc(datestamp).format('HH:mm:ss');
  }

  getDuration(start_time, end_time) {
    return moment.duration(moment(end_time).diff(moment(start_time))).humanize();
  }

  onDownloadRunsReport() {
    const format = 'csv'; // or 'json'
    const filename = `${moment.utc().format(DateTime.REPORT_DATE_TIME)}.${format}`;

    const onComplete = () => console.warn('completed downloading report');
    const onError = _e => console.error('error downloading report');
    const types = {'json': 'application/json', 'csv': 'text/csv'};
    const onNext = data => {
      const type = types[format];
      const blob = new Blob([data], {type});
      saveAs(blob, filename);
    };

    this.nodeRunsService.downloadRuns(format, this.nodeHistoryStore.filter.getValue()).pipe(
      finalize(onComplete))
      .subscribe(onNext, onError);
  }

  // return specific stat values
  stats(value) {
    switch (value) {
      case SelectedStatus.All:
        return this.nodeRunsCount.total;
      case SelectedStatus.Failure:
        return this.nodeRunsCount.failure;
      case SelectedStatus.Success:
        return this.nodeRunsCount.success;
      default:
        console.error('RunHistoryComponent: No status type: ' + value);
        return SelectedStatus.All;
    }
  }

  onStatusChange(status) {
    this.selected = status;
    switch (status) {
      case SelectedStatus.Failure:
        this.nodeHistoryStore.addFilter('status', 'failure');
        return;
      case SelectedStatus.Success:
        this.nodeHistoryStore.addFilter('status', 'success');
        return;
      default:
        this.nodeHistoryStore.removeFilter('status');
        return;
    }
  }

  // react on click events for history items
  onSelect(history) {
    if (history && this.activeRunId !== history.runId) {
      this.activeRunId = history.runId;
      this.on_run_change.emit(new RunInfo(history.runId, history.endTime));
    }
  }

  // react on events for date elector, pass event to parent component
  dateSelected(selected: string) {
    const dateString = HistorySelection.startingTimestamp(selected);
    this.nodeHistoryStore.addFilter('startDate', this.formatDate(dateString));
  }

  private formatDate(dateString: string): string {
    return moment.utc(dateString).format('YYYY-MM-DD');
  }

  updatePageNumber(pageNumber) {
    this.currentPage = pageNumber;
    this.nodeHistoryStore.addFilter('page', pageNumber);
  }

  loadRunCounts(nodeHistoryCountsFilter: NodeHistoryCountsFilter) {
    this.nodeRunsService.getNodeRunCounts(nodeHistoryCountsFilter).then(nodeRunsCount => {
      this.nodeRunsCount = nodeRunsCount;
      this.changeDetectorRef.markForCheck();
    });
  }

  loadHistory(nodeHistoryFilter: NodeHistoryFilter) {
    this.nodeRunsService.getNodeRuns(nodeHistoryFilter).then(runs => {
      this.nodeHistory = runs;
      this.changeDetectorRef.markForCheck();
    });
  }

  closeRunHistory() {
    this.closeRunHistoryEvent.emit(null);
  }

}
