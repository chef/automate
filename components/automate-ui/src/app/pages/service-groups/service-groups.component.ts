import { map, takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable } from 'rxjs';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';
import { Chicklet, RollupServiceStatus, SortDirection } from '../../types/types';
import { EntityStatus } from '../../entities/entities';
import { UpdateServiceGroupFilters } from 'app/entities/service-groups/service-groups.actions';
import {
  ServiceGroup, ServiceGroupFilters, FieldDirection
} from '../../entities/service-groups/service-groups.model';
import {
  serviceGroupStatus, allServiceGroups, serviceGroupState
} from '../../entities/service-groups/service-groups.selector';
import { find, includes } from 'lodash/fp';

@Component({
  selector: 'app-service-groups',
  templateUrl: './service-groups.component.html',
  styleUrls: ['./service-groups.component.scss']
})

export class ServiceGroupsComponent implements OnInit, OnDestroy {
  public serviceGroups$: Observable<ServiceGroup[]>;
  public serviceGroupStatus$: Observable<EntityStatus>;

  // The collection of allowable status
  private allowedStatus = ['ok', 'critical', 'warning', 'unknown'];

  // The currently selected health status filter
  selectedStatus$: Observable<string>;

  // Has this component been destroyed
  private isDestroyed: Subject<boolean> = new Subject();

  selectedFieldDirection$: Observable<SortDirection>;
  selectedSortField$: Observable<string>;
  currentPage$: Observable<number>;

  currentFieldDirection: SortDirection;
  currentSortField: string;

  defaultFieldDirection: FieldDirection = {
    name: 'ASC',
    percent_ok: 'ASC'
  };

  currentPage = 1;
  // The number of service groups to display per page
  pageSize = 25;
  // TODO: Wire this up with real data
  totalServiceGroups = 50;
  // The collection of allowable sort directions
  private allowedSortDirections = ['asc', 'desc', 'ASC', 'DESC'];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    const allUrlParameters$ = this.getAllUrlParameters();

    allUrlParameters$.pipe(takeUntil(this.isDestroyed)).subscribe(
      allUrlParameters => this.updateAllFilters(allUrlParameters));

    this.serviceGroupStatus$ = this.store.select(serviceGroupStatus);
    this.serviceGroups$ = this.store.select(allServiceGroups);

    this.selectedStatus$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.status));

    this.selectedFieldDirection$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.sortDirection));

    this.selectedFieldDirection$.subscribe(currentFieldDirection =>
      this.currentFieldDirection = currentFieldDirection);

    this.selectedSortField$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.sortField));

    this.selectedSortField$.subscribe(currentSortField =>
      this.currentSortField = currentSortField);

    this.currentPage$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.page));

    this.currentPage$.subscribe(currentPage => this.currentPage = currentPage);

  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public updateAllFilters(allParameters: Chicklet[]): void {
    const status = this.getSelectedStatus(allParameters);
    const sortDirection = this.getSortDirection(allParameters);
    const sortField = this.getSelectedSortField(allParameters);
    const pageField = this.getSelectedPageNumber(allParameters);
    // Here we can add all the filters that the search bar will have
    const serviceGroupFilters: ServiceGroupFilters = {
      status: status,
      sortField: sortField,
      sortDirection: sortDirection,
      page: pageField,
      pageSize: this.pageSize
    };
    this.store.dispatch(new UpdateServiceGroupFilters({filters: serviceGroupFilters}));
  }

  public statusFilter(status) {
    const queryParams = {...this.route.snapshot.queryParams};
    if ( includes(status, this.allowedStatus) ) {
      queryParams['status'] = [status];
    } else {
      delete queryParams['status'];
    }

    delete queryParams['page'];

    this.router.navigate([], {queryParams});
  }

  private getSelectedStatus(allParameters: Chicklet[]): RollupServiceStatus {
    const status = find((chicklet) => {
        return chicklet.type === 'status';
      }, allParameters);

    if (status !== undefined && includes(status.text, this.allowedStatus)) {
      return status.text as RollupServiceStatus;
    } else {
      return undefined;
    }
  }

  private getAllUrlParameters(): Observable<Chicklet[]> {
    return this.route.queryParamMap.pipe(map((params: ParamMap) => {
      return params.keys.reduce((list, key) => {
        const paramValues = params.getAll(key);
        return list.concat(paramValues.map(value => ({type: key, text: value})));
      }, []);
    }));
   }

  onPageChange(pageNumber: number) {
    if (pageNumber > 1 ) {
      const queryParams = {...this.route.snapshot.queryParams, page: [pageNumber]};

      this.router.navigate([], {queryParams});
    } else if ( pageNumber === 1 ) {
      const queryParams = {...this.route.snapshot.queryParams};
      delete queryParams['page'];

      this.router.navigate([], {queryParams});
    }
  }

  onToggleSort(field: string) {
    if (this.currentSortField === field) {
      const fieldDirection = this.currentFieldDirection === 'ASC' ? 'DESC' : 'ASC';
      this.onUpdateSort({field: field, fieldDirection: fieldDirection});
    } else {
      this.onUpdateSort({field: field, fieldDirection: this.defaultFieldDirection[field]});
    }
  }

  onUpdateSort(event): void {
    const {field, fieldDirection} = event;
    if (this.defaultFieldDirection.hasOwnProperty(field) &&
      this.allowedSortDirections.includes(fieldDirection) ) {
      const queryParams = {...this.route.snapshot.queryParams,
        sortField: [field], sortDirection: [fieldDirection]};

      delete queryParams['page'];

      this.router.navigate([], {queryParams});
    }
  }

  sortIcon(field: string): string {

    if (field === this.currentSortField) {
      return 'sort-' + this.currentFieldDirection.toLowerCase();
    } else {
      return 'sort';
    }
  }
  private getSelectedPageNumber(allUrlParameters: Chicklet[]): number {
    const pageChicklet = find((chicklet) => {
      return chicklet.type === 'page';
    }, allUrlParameters);

    if (pageChicklet !== undefined) {
      const n = Number(pageChicklet.text);
      if ( !isNaN(n) && n > 0) {
        return n;
      } else {
        return 1;
      }
    } else {
      return 1;
    }
  }

  private getSelectedSortField(allUrlParameters: Chicklet[]): string {
    const sortField = find((chicklet) => chicklet.type === 'sortField', allUrlParameters);
    if ( sortField !== undefined && this.defaultFieldDirection.hasOwnProperty(sortField.text) ) {
      return sortField.text;
    } else {
      return 'name';
    }
  }

  private getSortDirection(allUrlParameters: Chicklet[]): 'ASC' | 'DESC' {
    const sortDirection = find( (chicklet) => {
        return chicklet.type === 'sortDirection';
      }, allUrlParameters);

    return sortDirection !== undefined && sortDirection.text === 'DESC' ? 'DESC' : 'ASC';
  }
}
