import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { map, filter } from 'rxjs/operators';

import {
  GetDailyCheckInTimeSeries
} from 'app/entities/desktop/desktop.actions';

import {
  dailyCheckInCountCollection
} from 'app/entities/desktop/desktop.selectors';

import {
  DailyCheckInCount
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-daily-check-in',
  templateUrl: './daily-check-in.component.html',
  styleUrls: ['./daily-check-in.component.scss']
})
export class DailyCheckInComponent implements OnInit, OnDestroy {

  private isDestroyed = new Subject<boolean>();
  public last24HourCheckInCount$: Observable<DailyCheckInCount>;
  public unknownPercentage$: Observable<number>;
  public checkedInPercentage$: Observable<number>;
  public totalCount$: Observable<number>;
  public unknownCount$: Observable<number>;
  public checkedInCount$: Observable<number>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.store.dispatch(new GetDailyCheckInTimeSeries());

    this.last24HourCheckInCount$ = this.store.select(dailyCheckInCountCollection).pipe(
      filter(collection => collection.buckets.length > 0),
      map(collection => collection.buckets[0]));

    this.unknownPercentage$ = this.last24HourCheckInCount$.pipe(
        map(count => ((count.total - count.checkInCount) / count.total) * 100)
      );

    this.checkedInPercentage$ = this.last24HourCheckInCount$.pipe(
      map(count => (count.checkInCount / count.total) * 100)
    );

    this.totalCount$ = this.last24HourCheckInCount$.pipe(
      map(count => count.total)
    );

    this.checkedInCount$ = this.last24HourCheckInCount$.pipe(
      map(count => count.checkInCount)
    );

    this.unknownCount$ = this.last24HourCheckInCount$.pipe(
      map(count => count.total - count.checkInCount)
    );
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
