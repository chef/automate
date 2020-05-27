import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, DailyNodeRunsStatus } from 'app/entities/desktop/desktop.model';
import { DateTime } from 'app/helpers/datetime/datetime';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { GetDailyNodeRunsStatusTimeSeries } from 'app/entities/desktop/desktop.actions';
import { getDailyNodeRuns } from 'app/entities/desktop/desktop.selectors';

// const checkinHistory = Array.from(new Array(29)).map((_v, i) => {
//   const historyType = historyTypes[Math.floor(Math.random() * historyTypes.length)];
//   const date = new Date();
//   return { ...historyType, date: new Date(date.setDate(date.getDate() - i)) };
// });

@Component({
  selector: 'app-desktop-detail',
  templateUrl: './desktop-detail.component.html',
  styleUrls: ['./desktop-detail.component.scss']
})
export class DesktopDetailComponent implements OnInit {

  @Input() desktop: Desktop;
  @Input() fullscreened = false;

  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() fullscreenToggled: EventEmitter<void> = new EventEmitter();

  public checkInHistory: DailyNodeRunsStatus[];
  public ceil = Math.ceil;
  public DateTime = DateTime;
  public showCheckinDebug = false;
  public checkinTableType = 'grid';
  public checkinGridFlexType = 'wrap';
  public checkinNumDays = 14;
  public historyIcons = {
    converged: 'check_box',
    unchanged: 'indeterminate_check_box',
    failure: 'warning',
    error: 'warning',
    unknown: 'help',
    missing: 'help'
  };

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.store.select(getDailyNodeRuns).subscribe((dailyNodeRuns) => {
      this.checkInHistory = this.addCheckInLabels(dailyNodeRuns.durations.buckets);
    });
  }

  ngOnInit() {
    this.getCheckInHistory();
  }

  getCheckInHistory() {
    this.store.dispatch(new GetDailyNodeRunsStatusTimeSeries(this.desktop.id, this.checkinNumDays));
  }

  updateCheckInDays() {
    this.checkinNumDays = (this.checkinNumDays === 14 ? 28 : 14);
    this.getCheckInHistory();
  }

  addCheckInLabels(checkInHistory: DailyNodeRunsStatus[]): DailyNodeRunsStatus[] {
    let numWeeks = Math.floor(checkInHistory.length / 7);
    checkInHistory.forEach((history: DailyNodeRunsStatus, index: number) => {
      const isStartOfWeek = index % 7 === 0;
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
