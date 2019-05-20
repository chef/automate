import { takeUntil, withLatestFrom, distinctUntilChanged } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable, combineLatest } from 'rxjs';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ActivatedRoute, Router } from '@angular/router';
import { Chicklet, RollupServiceStatus, SortDirection } from '../../types/types';
import { EntityStatus } from '../../entities/entities';
import {
  UpdateServiceGroupFilters, UpdateSelectedSG
} from 'app/entities/service-groups/service-groups.actions';
import {
  ServiceGroup, ServiceGroupFilters, FieldDirection, HealthSummary, ServicesFilters
} from '../../entities/service-groups/service-groups.model';
import {
  serviceGroupStatus, allServiceGroups, serviceGroupState, allServiceGroupHealth
} from '../../entities/service-groups/service-groups.selector';
import { find, includes, get } from 'lodash/fp';

@Component({
  selector: 'app-service-groups',
  templateUrl: './service-groups.component.html',
  styleUrls: ['./service-groups.component.scss']
})

export class ServiceGroupsComponent implements OnInit, OnDestroy {
  public serviceGroups$: Observable<ServiceGroup[]>;
  public serviceGroupStatus$: Observable<EntityStatus>;
  public sgHealthSummary: HealthSummary;

  // The selected service-group id that will be sent to the services-sidebar
  public selectedServiceGroupId: string;

  // The current page the user is visualizing
  public currentPage = 1;

  // The number of service groups to display per page
  public pageSize = 25;

  // Total number of service groups
  public totalServiceGroups = 0;

  // The currently selected health status filter
  public selectedStatus$: Observable<string>;

  // The collection of allowable status
  private allowedStatus = ['ok', 'critical', 'warning', 'unknown'];

  // Has this component been destroyed
  private isDestroyed: Subject<boolean> = new Subject();

  // The collection of allowable sort directions
  private allowedSortDirections = ['asc', 'desc', 'ASC', 'DESC'];

  private selectedFieldDirection$: Observable<SortDirection>;
  private selectedSortField$: Observable<string>;
  private healthSummary$: Observable<HealthSummary>;
  private currentPage$: Observable<number>;
  private currentFieldDirection: SortDirection;
  private currentSortField: string;
  private defaultFieldDirection: FieldDirection = {
    name: 'ASC',
    percent_ok: 'ASC'
  };

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit() {
    this.route.queryParamMap.pipe(
      distinctUntilChanged((a, b) => {
        return a.get('status') === b.get('status') &&
               a.get('page') === b.get('page') &&
               a.get('sortField') === b.get('sortField') &&
               a.get('sortDirection') === b.get('sortDirection');
      }),
      takeUntil(this.isDestroyed)
    ).subscribe(queryParams => this.listParamsChange(queryParams));

    combineLatest(
      this.route.queryParamMap.pipe(
        distinctUntilChanged((a, b) => {
          return a.get('sgId') === b.get('sgId') &&
                 a.get('sgStatus') === b.get('sgStatus') &&
                 a.get('sgPage') === b.get('sgPage');
        })
      ),
      this.store.select(allServiceGroups)
    )
    .pipe(takeUntil(this.isDestroyed))
    .subscribe(([queryParams]) => this.detailParamsChange(queryParams));

    this.serviceGroupStatus$ = this.store.select(serviceGroupStatus);
    this.serviceGroups$ = this.store.select(allServiceGroups);
    this.serviceGroups$.pipe(
      withLatestFrom(this.route.queryParamMap),
      takeUntil(this.isDestroyed)
    )
    .subscribe(([serviceGroups, queryParams]) => {
      if (serviceGroups.length > 0) {
        const sgId = queryParams.get('sgId') || serviceGroups[0]['id'];
        this.router.navigate([], { queryParams: { sgId }, queryParamsHandling: 'merge' });
      } else {
        this.selectedServiceGroupId = null;
      }
    });

    this.healthSummary$ = this.store.select(allServiceGroupHealth);
    this.healthSummary$.pipe(takeUntil(this.isDestroyed))
      .subscribe(sgHealthSummary => this.sgHealthSummary = sgHealthSummary);

    this.selectedStatus$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.status));
    this.selectedStatus$.pipe(takeUntil(this.isDestroyed)).subscribe((status) => {
      // This code enables the pagination of service groups correctly, when the user selects
      // a Health Filter, we adjust the total number of service groups
      if ( includes(status, this.allowedStatus) ) {
          this.totalServiceGroups = get(status, this.sgHealthSummary);
      } else {
          this.totalServiceGroups = get('total', this.sgHealthSummary);
      }
    });

    this.selectedFieldDirection$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.sortDirection));

    this.selectedFieldDirection$.pipe(takeUntil(this.isDestroyed))
      .subscribe(currentFieldDirection => this.currentFieldDirection = currentFieldDirection);

    this.selectedSortField$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.sortField));

    this.selectedSortField$.pipe(takeUntil(this.isDestroyed)).subscribe(currentSortField =>
      this.currentSortField = currentSortField);

    this.currentPage$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.page));

    this.currentPage$.pipe(takeUntil(this.isDestroyed))
      .subscribe(currentPage => this.currentPage = currentPage);
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  listParamsChange(queryParams) {
    const allParameters = queryParams.keys.reduce((list, key) => {
      return list.concat(queryParams.getAll(key).map(value => ({ type: key, text: value })));
    }, []);
    this.updateAllFilters(allParameters);
  }

  detailParamsChange(queryParams) {
    const sgId = queryParams.get('sgId');
    if (sgId) {
      const servicesFilters: ServicesFilters = {
        service_group_id: sgId,
        page: parseInt(queryParams.get('sgPage'), 10) || 1,
        pageSize: parseInt(queryParams.get('sgPageSize'), 10) || 25,
        health: queryParams.get('sgStatus') || 'total'
      };
      this.updateServicesSidebar(servicesFilters);
    }
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
    delete queryParams['sgId'];
    delete queryParams['sgPage'];
    delete queryParams['sgStatus'];

    this.router.navigate([], {queryParams});
  }

  public onServiceGroupSelect(event: Event, id: string) {
    event.preventDefault();

    const queryParams = { ...this.route.snapshot.queryParams };
    queryParams['sgId'] = id;
    delete queryParams['sgPage'];
    delete queryParams['sgStatus'];
    this.router.navigate([], { queryParams });
  }

  public updateServicesSidebar(servicesFilters: ServicesFilters) {
    this.selectedServiceGroupId = servicesFilters.service_group_id;
    this.store.dispatch(new UpdateSelectedSG(servicesFilters));
    document.querySelector<HTMLElement>('app-services-sidebar').focus();
  }

  private getSelectedStatus(allParameters: Chicklet[]): RollupServiceStatus {
    const status = find((chicklet) => {
        return chicklet.type === 'status';
      }, allParameters);

    if (status !== undefined && includes(status.text, this.allowedStatus)) {
      return status.text as RollupServiceStatus;
    }
    return undefined;
  }

  onPageChange(pageNumber: number) {
    const queryParams = { ...this.route.snapshot.queryParams, page: pageNumber };

    if (pageNumber <= 1) {
      delete queryParams['page'];
    }

    delete queryParams['sgId'];
    delete queryParams['sgPage'];
    delete queryParams['sgStatus'];

    this.router.navigate([], { queryParams });
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
      delete queryParams['sgId'];
      delete queryParams['sgPage'];
      delete queryParams['sgStatus'];

      this.router.navigate([], {queryParams});
    }
  }

  sortIcon(field: string): string {
    if (field === this.currentSortField) {
      return 'sort-' + this.currentFieldDirection.toLowerCase();
    }
    return 'sort';
  }

  private getSelectedPageNumber(allUrlParameters: Chicklet[]): number {
    const pageChicklet = find((chicklet) => {
      return chicklet.type === 'page';
    }, allUrlParameters);

    if (pageChicklet !== undefined) {
      const n = Number(pageChicklet.text);
      if ( !isNaN(n) && n > 0) {
        return n;
      }
    }
    return 1;
  }

  private getSelectedSortField(allUrlParameters: Chicklet[]): string {
    const sortField = find((chicklet) => chicklet.type === 'sortField', allUrlParameters);
    if ( sortField !== undefined && this.defaultFieldDirection.hasOwnProperty(sortField.text) ) {
      return sortField.text;
    }
    return 'name';
  }

  private getSortDirection(allUrlParameters: Chicklet[]): 'ASC' | 'DESC' {
    const sortDirection = find( (chicklet) => {
        return chicklet.type === 'sortDirection';
      }, allUrlParameters);

    return sortDirection !== undefined && sortDirection.text === 'DESC' ? 'DESC' : 'ASC';
  }
}
