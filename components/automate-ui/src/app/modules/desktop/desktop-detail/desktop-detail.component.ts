import { Component, OnInit, OnDestroy, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, DailyNodeRunsStatus } from 'app/entities/desktop/desktop.model';
import { takeUntil } from 'rxjs/operators';
import { DateTime } from 'app/helpers/datetime/datetime';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Subject, Subscription } from 'rxjs';
import { GetDailyNodeRunsStatusTimeSeries } from 'app/entities/desktop/desktop.actions';
import { getDailyNodeRuns } from 'app/entities/desktop/desktop.selectors';

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
  public ceil = Math.ceil;
  public DateTime = DateTime;
  public showCheckinDebug = false;
  public checkinTableType = 'grid';
  public checkinGridFlexType = 'wrap';
  public checkinNumDays = 14;
  // These are Material Icon names from https://material.io/resources/icons/
  public historyIcons = {
    converged: 'check_box',
    unchanged: 'indeterminate_check_box',
    failure: 'warning',
    error: 'warning',
    unknown: 'help',
    missing: 'help'
  };
  private subscription: Subscription;
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.subscription = this.store.select(getDailyNodeRuns).pipe(
      takeUntil(this.isDestroyed)
      ).subscribe((dailyNodeRuns) => {
      this.checkInHistory = this.addCheckInLabels(dailyNodeRuns.durations.buckets);
    });
  }

  ngOnInit() {
    this.getCheckInHistory();
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
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
