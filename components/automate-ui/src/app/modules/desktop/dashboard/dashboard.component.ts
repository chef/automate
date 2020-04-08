import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { map, filter } from 'rxjs/operators';
import { last, reverse } from 'lodash/fp';

import {
  GetDailyCheckInTimeSeries,
  SetDaysAgoSelected,
  GetTopErrorsCollection,
  GetUnknownDesktopDurationCounts,
  GetDesktops,
  GetDesktopsTotal,
  UpdateDesktopFilterCurrentPage,
  UpdateDesktopFilterTerm,
  RemoveDesktopFilterTerm
} from 'app/entities/desktop/desktop.actions';
import {
  dailyCheckInCountCollection,
  getSelectedDaysAgo,
  topErrorsCollection,
  unknownDesktopDurationCounts,
  desktops,
  desktopsTotal,
  desktopsCurrentPage,
  desktopsPageSize,
  desktopsFilterTerms
} from 'app/entities/desktop/desktop.selectors';
import {
  DailyCheckInCount, DailyCheckInCountCollection, DayPercentage,
  TopErrorsItem, CountedDurationItem, Desktop, TermFilter
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
  public checkInCountCollectedUpdated$: Observable<Date>;
  public topErrorsUpdated$: Observable<Date>;
  public unknownDesktopCountedDurationItems$: Observable<CountedDurationItem[]>;
  public unknownDesktopCountedDurationUpdated$: Observable<Date>;
  public desktops$: Observable<Desktop[]>;
  public totalDesktopCount$: Observable<number>;
  public currentPage$: Observable<number>;
  public pageSize$: Observable<number>;
  public termFilters$: Observable<TermFilter[]>;
  public insightVisible = false;

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.store.dispatch(new GetDailyCheckInTimeSeries());
    this.store.dispatch(new GetTopErrorsCollection());
    this.store.dispatch(new GetUnknownDesktopDurationCounts());
    this.store.dispatch(new GetDesktops());
    this.store.dispatch(new GetDesktopsTotal());

    this.termFilters$ = this.store.select(desktopsFilterTerms);

    this.pageSize$ = this.store.select(desktopsPageSize);

    this.currentPage$ = this.store.select(desktopsCurrentPage);
    this.selectedDaysAgo$ = this.store.select(getSelectedDaysAgo);

    this.desktops$ = this.store.select(desktops);

    this.totalDesktopCount$ = this.store.select(desktopsTotal);

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

    this.checkInCountCollectedUpdated$ = this.checkInCountCollection$.pipe(
      map(collection => collection.updated)
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

    this.topErrorsUpdated$ = this.store.select(topErrorsCollection).pipe(
      map(collection => collection.updated)
    );

    this.unknownDesktopCountedDurationItems$ = this.store.select(unknownDesktopDurationCounts).pipe(
      map(counts => counts.items)
    );

    this.unknownDesktopCountedDurationUpdated$ =
      this.store.select(unknownDesktopDurationCounts).pipe(
      map(counts => counts.updated)
    );
  }

  handleDaysAgoChange(daysAgo: number) {
    this.store.dispatch(new SetDaysAgoSelected({daysAgo}));
  }

  insightClose() {
    this.insightVisible = false;
  }

  public onPageChange(pageNumber: number) {
    this.store.dispatch(new UpdateDesktopFilterCurrentPage({page: pageNumber}));
  }

  public onErrorSelected(errorItem: TopErrorsItem): void {
    const terms = [
      { type: 'error_message', value: errorItem.message },
      { type: 'error_type', value: errorItem.type }];
    this.store.dispatch(new UpdateDesktopFilterTerm({ terms }));
    this.insightVisible = true;
  }

  public ontermFilterSelected(term: TermFilter): void {
    this.store.dispatch(new RemoveDesktopFilterTerm({ term }));
  }
}
