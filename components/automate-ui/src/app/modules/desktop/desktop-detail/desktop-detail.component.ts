import { Component, OnInit, OnDestroy, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, DailyNodeRunsStatus } from 'app/entities/desktop/desktop.model';
import { takeUntil, finalize } from 'rxjs/operators';
import * as moment from 'moment/moment';
import { saveAs } from 'file-saver';
import { DateTime } from 'app/helpers/datetime/datetime';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Subject } from 'rxjs';
import { GetDailyNodeRunsStatusTimeSeries } from 'app/entities/desktop/desktop.actions';
import { getDailyNodeRuns } from 'app/entities/desktop/desktop.selectors';
import { NodeRunsService } from '../../../services/node-details/node-runs.service';
import { RunHistoryStore } from '../../../services/run-history-store/run-history.store';

@Component({
  selector: 'app-desktop-detail',
  templateUrl: './desktop-detail.component.html',
  styleUrls: ['./desktop-detail.component.scss']
})
export class DesktopDetailComponent implements OnInit, OnDestroy {

  @Input() desktop: Desktop;
  @Input() fullscreened = false;

  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() fullscreenToggled: EventEmitter<void> = new EventEmitter();

  public checkInHistory: DailyNodeRunsStatus[];
  public DateTime = DateTime;
  public showCheckinDebug = false;
  public checkinTableType = 'grid';
  public checkinGridFlexType = 'wrap';
  public checkinNumDays = 15;
  public downloadDropdownVisible = false;
  // These are Material Icon names from https://material.io/resources/icons/
  public historyIcons = {
    converged: 'check_box',
    unchanged: 'indeterminate_check_box',
    failure: 'warning',
    error: 'warning',
    unknown: 'help',
    missing: 'help'
  };
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private nodeHistoryStore: RunHistoryStore,
    private nodeRunsService: NodeRunsService
  ) {
    this.store.select(getDailyNodeRuns).pipe(
      takeUntil(this.isDestroyed)
      ).subscribe((dailyNodeRuns) => {
      this.checkInHistory = this.addCheckInLabels(dailyNodeRuns.durations.buckets);
    });
  }

  ngOnInit() {
    this.getCheckInHistory();
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getCheckInHistory() {
    this.store.dispatch(new GetDailyNodeRunsStatusTimeSeries(this.desktop.id, this.checkinNumDays));
  }

  updateCheckInDays() {
    this.checkinNumDays = (this.checkinNumDays === 15 ? 29 : 15);
    this.getCheckInHistory();
  }

  onDownloadCheckInHistory(format) {
    this.closeDownloadDropdown();
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

  toggleDownloadDropdown() {
    this.downloadDropdownVisible = !this.downloadDropdownVisible;
  }

  closeDownloadDropdown() {
    this.downloadDropdownVisible = false;
  }

  addCheckInLabels(checkInHistory: DailyNodeRunsStatus[]): DailyNodeRunsStatus[] {
    let numWeeks = Math.floor(checkInHistory.length / 7);
    checkInHistory.forEach((history: DailyNodeRunsStatus, index: number) => {
      const isStartOfWeek = (index % 7 === 0) && numWeeks > 0;
      const startOfWeekLabelText = numWeeks > 1 ? `${numWeeks} weeks ago` : `${numWeeks} week ago`;
      const isToday = index === (checkInHistory.length - 1);
      const labelText = isToday ? 'Today' : '';
      history.label = isStartOfWeek ? startOfWeekLabelText : labelText;
      if (isStartOfWeek) { --numWeeks; }
    });
    return checkInHistory;
  }

  public close(): void {
    this.closed.emit();
  }
}
