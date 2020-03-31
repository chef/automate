import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { map, filter } from 'rxjs/operators';
import { last, reverse } from 'lodash/fp';

import {
  GetDailyCheckInTimeSeries,
  SetDaysAgoSelected,
  GetTopErrorsCollection
} from 'app/entities/desktop/desktop.actions';

import {
  dailyCheckInCountCollection,
  getSelectedDaysAgo,
  topErrorsCollection
} from 'app/entities/desktop/desktop.selectors';

import {
  DailyCheckInCount, DailyCheckInCountCollection, DayPercentage, TopErrorsItem
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-desktop-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit {

  private checkInCountCollection$: Observable<DailyCheckInCountCollection>;
  private last24HourCheckInCount$: Observable<DailyCheckInCount>;
  public unknownPercentage$: Observable<number>;
  public checkedInPercentage$: Observable<number>;
  public totalCount$: Observable<number>;
  public unknownCount$: Observable<number>;
  public checkedInCount$: Observable<number>;
  public days$: Observable<DayPercentage[]>;
  public selectedDaysAgo$: Observable<number>;
  public topErrorsItems$: Observable<TopErrorsItem[]>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.store.dispatch(new GetDailyCheckInTimeSeries());
    this.store.dispatch(new GetTopErrorsCollection());

    this.selectedDaysAgo$ = this.store.select(getSelectedDaysAgo);

    this.checkInCountCollection$ = this.store.select(dailyCheckInCountCollection).pipe(
      filter(collection => collection.buckets.length > 0));

    this.days$ = this.checkInCountCollection$.pipe(
      map(collection =>
        reverse(collection.buckets).map((bucket, index) => {
          let percentage = 100;
          if (bucket.total > 0) {
            percentage = (bucket.checkInCount / bucket.total) * 100;
          }
          return {daysAgo: index, percentage: percentage};
        }))
    );

    this.last24HourCheckInCount$ = this.checkInCountCollection$.pipe(
      map(collection => last(collection.buckets))
    );

    this.unknownPercentage$ = this.last24HourCheckInCount$.pipe(
        map(count => {
          if (count.total > 0) {
            return ((count.total - count.checkInCount) / count.total) * 100;
          }
          return 100;
        })
      );

    this.checkedInPercentage$ = this.last24HourCheckInCount$.pipe(
      map(count => {
        if (count.total > 0) {
          return (count.checkInCount / count.total) * 100;
        }
        return 100;
      })
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

    this.topErrorsItems$ = this.store.select(topErrorsCollection).pipe(
      map(collection => collection.items)
    );
  }

  handleDaysAgoChange(daysAgo: number) {
    this.store.dispatch(new SetDaysAgoSelected({daysAgo}));
  }
}
