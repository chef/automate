import { Component, OnInit } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { map, filter } from 'rxjs/operators';
import { last, reverse } from 'lodash/fp';
import * as moment from 'moment/moment';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NodeRun } from 'app/types/types';

import {
  GetDailyCheckInTimeSeries,
  SetSelectedDesktop,
  GetDesktop,
  SetSelectedDaysAgo,
  GetTopErrorsCollection,
  GetUnknownDesktopDurationCounts,
  UpdateDesktopListTitle,
  GetDesktops,
  GetDesktopsTotal,
  UpdateDesktopFilterCurrentPage,
  UpdateDesktopFilterTerm,
  UpdateDesktopColumnOptions,
  GetNodeMetadataCounts,
  AddDesktopFilterTerm,
  RemoveDesktopFilterTerm,
  UpdateDesktopSortTerm,
  UpdateDesktopDateTerm,
  UpdateDesktopsFilterPageSizeAndCurrentPage,
  GetDailyNodeRunsStatusTimeSeries,
  GetDesktopColumnOptionsDefaults
} from 'app/entities/desktop/desktop.actions';
import {
  dailyCheckInCountCollection,
  getSelectedDaysAgo,
  getDailyNodeRuns,
  topErrorsCollection,
  unknownDesktopDurationCounts,
  nodeMetadataCounts,
  desktopListTitle,
  desktopListColumns,
  desktopListColumnsSaveAsDefault,
  desktops,
  desktopsTotal,
  desktopsCurrentPage,
  desktopsPageSize,
  desktopsFilterTerms,
  getSelectedNodeRun
} from 'app/entities/desktop/desktop.selectors';
import {
  DailyCheckInCount, DailyCheckInCountCollection, DayPercentage,
  TopErrorsItem, CountedDurationItem, Desktop, TermFilter, Terms,
  NodeMetadataCount, DailyNodeRuns, PageSizeChangeEvent, DesktopColumnOption,
  DesktopColumnOptionUpdate
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-desktop-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit {

  private checkInCountCollection$: Observable<DailyCheckInCountCollection>;
  public last24HourCheckInCount$: Observable<DailyCheckInCount>;
  public unknownPercentage$: Observable<number>;
  public checkedInPercentage$: Observable<number>;
  public totalCount$: Observable<number>;
  public unknownCount$: Observable<number>;
  public checkedInCount$: Observable<number>;
  public days$: Observable<DayPercentage[]>;
  public selectedDaysAgo$: Observable<number>;
  public checkInHistory$: Observable<DailyNodeRuns>;
  public topErrorsItems$: Observable<TopErrorsItem[]>;
  public checkInCountCollectedUpdated$: Observable<Date>;
  public topErrorsUpdated$: Observable<Date>;
  public unknownDesktopCountedDurationItems$: Observable<CountedDurationItem[]>;
  public unknownDesktopCountedDurationUpdated$: Observable<Date>;
  public nodeMetadataCounts$: Observable<NodeMetadataCount[]>;
  public desktopListTitle$: Observable<string>;
  public desktopListColumns$: Observable<DesktopColumnOption[]>;
  public desktopListColumnsSaveAsDefault$: Observable<boolean>;
  public desktops$: Observable<Desktop[]>;
  public totalDesktopCount$: Observable<number>;
  public currentPage$: Observable<number>;
  public pageSize$: Observable<number>;
  public termFilters$: Observable<TermFilter[]>;
  public selectedNodeRun$: Observable<NodeRun>;
  public checkInNumDays = 15;
  public desktopListVisible = false;
  public desktopListFullscreened = false;
  public desktopDetailVisible = false;
  public desktopDetailFullscreened = false;
  public selectedDesktop: Desktop;
  public selectedCheckInStatus: string;
  public selectedDuration: string;
  public selectedError: TopErrorsItem;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService
  ) { }

  ngOnInit() {
    this.store.dispatch(new GetDailyCheckInTimeSeries());
    this.store.dispatch(new GetTopErrorsCollection());
    this.store.dispatch(new GetUnknownDesktopDurationCounts());
    this.store.dispatch(new GetDesktops());
    this.store.dispatch(new GetDesktopsTotal());
    this.store.dispatch(new GetNodeMetadataCounts());
    this.store.dispatch(new GetDesktopColumnOptionsDefaults());

    this.nodeMetadataCounts$ = this.store.select(nodeMetadataCounts);

    this.termFilters$ = this.store.select(desktopsFilterTerms);

    this.pageSize$ = this.store.select(desktopsPageSize);

    this.currentPage$ = this.store.select(desktopsCurrentPage);
    this.selectedDaysAgo$ = this.store.select(getSelectedDaysAgo);

    this.desktopListTitle$ = this.store.select(desktopListTitle);
    this.desktopListColumns$ = this.store.select(desktopListColumns);
    this.desktopListColumnsSaveAsDefault$ = this.store.select(desktopListColumnsSaveAsDefault);
    this.desktops$ = this.store.select(desktops);

    this.selectedNodeRun$ = this.store.select(getSelectedNodeRun);

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
          return {daysAgo: index, percentage: percentage, total: bucket.total};
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

    this.checkInHistory$ = this.store.select(getDailyNodeRuns);

    setTimeout(() => this.layoutFacade.hideSidebar());
  }

  handleDaysAgoChange(daysAgo: number) {
    this.store.dispatch(new SetSelectedDaysAgo({daysAgo}));
  }

  onDesktopListClose() {
    this.onDesktopDetailClose();
    this.selectedCheckInStatus = '';
    this.selectedDuration = undefined;
    this.selectedError = undefined;
    this.desktopListFullscreened = false;
    this.desktopListVisible = false;
    this.store.dispatch(new UpdateDesktopDateTerm({}));
    this.store.dispatch(new UpdateDesktopFilterTerm({ terms: [] }));
    this.store.dispatch(new UpdateDesktopDateTerm({}));
  }

  onDesktopListFullscreen() {
    this.desktopListFullscreened = !this.desktopListFullscreened;
    this.desktopDetailVisible = !this.desktopDetailVisible;
  }

  onDesktopDetailClose() {
    this.selectedDesktop = undefined;
    this.desktopDetailFullscreened = false;
    this.desktopDetailVisible = false;
    this.desktopListVisible = true;
  }

  onDesktopDetailFullscreen() {
    this.desktopDetailFullscreened = !this.desktopDetailFullscreened;
    this.desktopListVisible = !this.desktopListVisible;
  }

  public onPageChange(pageNumber: number) {
    this.store.dispatch(new UpdateDesktopFilterCurrentPage({page: pageNumber}));
  }

  public onDailyStatusSelected(status: string, last24HourCheckInCount: DailyCheckInCount) {
    if (!status) { return this.onDesktopListClose(); }

    let start: Date;
    let end: Date;

    switch (status) {
      case ('checked-in'):
        start = new Date(last24HourCheckInCount.start);
        end = new Date(last24HourCheckInCount.end);
        break;
      case ('unknown'):
        end = new Date(last24HourCheckInCount.start);
        break;
    }

    this.store.dispatch(new UpdateDesktopListTitle(`Daily Check-in: ${status}`));
    this.store.dispatch(new UpdateDesktopDateTerm({ start, end }));
    this.store.dispatch(new UpdateDesktopFilterTerm({ terms: [] }));
    this.store.dispatch(new GetNodeMetadataCounts());
    this.store.dispatch(new GetDesktops());
    this.selectedCheckInStatus = status;
    this.selectedError = undefined;
    this.selectedDuration = undefined;
    this.desktopListVisible = true;
  }

  public onTermFilterAdded(term: TermFilter) {
    this.store.dispatch(new AddDesktopFilterTerm({ term }));
    this.store.dispatch(new GetNodeMetadataCounts());
    this.store.dispatch(new GetDesktops());
  }

  public onTermFilterRemoved(term: TermFilter) {
    this.store.dispatch(new RemoveDesktopFilterTerm({ term }));
    this.store.dispatch(new GetNodeMetadataCounts());
    this.store.dispatch(new GetDesktops());
  }

  public onPageSizeChange(event: PageSizeChangeEvent) {
    this.store.dispatch(new UpdateDesktopsFilterPageSizeAndCurrentPage({
      pageSize: event.pageSize,
      updatedPageNumber: event.updatedPageNumber
    }));
  }

  public onErrorSelected(error: TopErrorsItem): void {
    if (!error) { return this.onDesktopListClose(); }

    const terms = [
      { type: Terms.ErrorType, value: error.type },
      { type: Terms.ErrorMessage, value: error.message },
      { type: Terms.Status, value: 'failure' }
    ];
    this.store.dispatch(new UpdateDesktopListTitle(`${error.type}: ${error.message}`));
    this.store.dispatch(new UpdateDesktopDateTerm({}));
    this.store.dispatch(new UpdateDesktopFilterTerm({ terms }));
    this.store.dispatch(new GetNodeMetadataCounts());
    this.store.dispatch(new GetDesktops());
    this.selectedError = error;
    this.selectedDuration = undefined;
    this.selectedCheckInStatus = '';
    this.desktopListVisible = true;
  }

  public onDurationSelected(duration: string): void {
    if (!duration) { return this.onDesktopListClose(); }

    const units = { 'M': 'month', 'w': 'week', 'd': 'day' };
    const unit = units[duration[1]];
    const amount = parseInt(duration[0], 10);
    const end = moment().utc().subtract(amount, unit).toDate();
    this.store.dispatch(new UpdateDesktopListTitle(`Last Check-in: ${amount} ${unit}${amount !== 1 ? 's' : ''} ago`));
    this.store.dispatch(new UpdateDesktopDateTerm({ end }));
    this.store.dispatch(new UpdateDesktopFilterTerm({ terms: [] }));
    this.store.dispatch(new GetNodeMetadataCounts());
    this.store.dispatch(new GetDesktops());
    this.selectedDuration = duration;
    this.selectedError = undefined;
    this.selectedCheckInStatus = '';
    this.desktopListVisible = true;
  }

  public onDesktopSelected(desktop: Desktop) {
    this.selectedDesktop = desktop;
    this.store.dispatch(new SetSelectedDesktop({desktop}));
    this.store.dispatch(new GetDesktop({ nodeId: desktop.id, runId: desktop.latestRunId }));
    this.store.dispatch(
      new GetDailyNodeRunsStatusTimeSeries(this.selectedDesktop.id, this.checkInNumDays));
    this.desktopDetailVisible = true;
    this.desktopListFullscreened = false;
  }

  public onCheckInNumDaysChanged(checkInNumDays: number) {
    this.checkInNumDays = checkInNumDays;
    this.store.dispatch(
      new GetDailyNodeRunsStatusTimeSeries(this.selectedDesktop.id, this.checkInNumDays));
  }

  public onTermFilterSelected(term: TermFilter): void {
    this.store.dispatch(new RemoveDesktopFilterTerm({ term }));
  }

  public onSortChange(insightField: string): void {
    const term = this.getTerm(insightField);
    this.store.dispatch(new UpdateDesktopSortTerm({ term }));
  }

  public onDesktopColumnOptionsUpdated(event: DesktopColumnOptionUpdate) {
    this.store.dispatch(new UpdateDesktopColumnOptions(event));
  }

  private getTerm(field: string): string {
    switch (field) {
      case 'name':
        return Terms.DesktopName;
      case 'check-in':
        return Terms.CheckInTime;
      case 'platform':
        return Terms.Platform;
      default:
        return field;
    }
  }
}
