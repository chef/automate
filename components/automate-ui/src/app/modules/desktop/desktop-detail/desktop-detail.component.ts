import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, DailyNodeRuns, DailyNodeRunsStatus } from 'app/entities/desktop/desktop.model';
import { finalize } from 'rxjs/operators';
import * as moment from 'moment/moment';
import { saveAs } from 'file-saver';
import { DateTime } from 'app/helpers/datetime/datetime';
import { NodeRunsService } from 'app/services/node-details/node-runs.service';
import { RunHistoryStore } from 'app/services/run-history-store/run-history.store';
import { NodeRun } from 'app/types/types';

@Component({
  selector: 'app-desktop-detail',
  templateUrl: './desktop-detail.component.html',
  styleUrls: ['./desktop-detail.component.scss']
})
export class DesktopDetailComponent {

  @Input() desktop: Desktop;
  @Input() nodeRun: NodeRun;
  @Input() fullscreened = false;
  @Input() checkInHistory: DailyNodeRuns;
  @Input() checkInNumDays = 15;

  @Output() checkInNumDaysChanged: EventEmitter<number> = new EventEmitter();
  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() fullscreenToggled: EventEmitter<void> = new EventEmitter();

  public DateTime = DateTime;
  public showCheckinDebug = false;
  public downloadDropdownVisible = false;
  public downloadInProgress = false;
  public downloadFailed = false;
  public twoWeekNumDays = 15; // 14 days + 1 offset
  public fourWeekNumDays = 29; // 28 days + 1 offset
  // These are Material Icon names from https://material.io/resources/icons/
  public historyIcons = {
    success: 'check_box',
    converged: 'check_box',
    unchanged: 'indeterminate_check_box',
    failure: 'warning',
    error: 'warning',
    unknown: 'help',
    missing: 'help'
  };

  constructor(
    private nodeHistoryStore: RunHistoryStore,
    private nodeRunsService: NodeRunsService
  ) {}

  updateCheckInDays() {
    const numDaysChanged =
      this.checkInNumDays === this.twoWeekNumDays ? this.fourWeekNumDays : this.twoWeekNumDays;
    this.checkInNumDaysChanged.emit(numDaysChanged);
  }

  onDownloadCheckInHistory(format) {
    this.closeDownloadDropdown();
    const filename = `${moment.utc().format(DateTime.REPORT_DATE_TIME)}.${format}`;

    const onComplete = () => this.downloadInProgress = false;
    const onError = _e => this.downloadFailed = true;
    const types = {'json': 'application/json', 'csv': 'text/csv'};
    const onNext = data => {
      const type = types[format];
      const blob = new Blob([data], {type});
      saveAs(blob, filename);
    };
    const filters = this.nodeHistoryStore.filter.getValue();
    filters.nodeId = this.desktop.id;

    this.nodeRunsService.downloadRuns(format, filters).pipe(
      finalize(onComplete))
      .subscribe(onNext, onError);
  }

  toggleDownloadDropdown() {
    this.downloadDropdownVisible = !this.downloadDropdownVisible;
  }

  closeDownloadDropdown() {
    this.downloadDropdownVisible = false;
  }

  get labeledCheckInHistory() {
    const buckets = this.checkInHistory.durations.buckets;
    let numWeeks = Math.floor(buckets.length / 7);
    return buckets.map((history: DailyNodeRunsStatus, index: number) => {
      const isStartOfWeek = (index % 7 === 0) && numWeeks > 0;
      const startOfWeekLabelText = `${numWeeks} week${numWeeks !== 1 ? 's' : ''} ago`;
      const isToday = index === (buckets.length - 1);
      const labelText = isToday ? 'Today' : '';
      history.label = isStartOfWeek ? startOfWeekLabelText : labelText;
      if (isStartOfWeek) { --numWeeks; }
      return history;
    });
  }

  public close(): void {
    this.closed.emit();
  }
}
