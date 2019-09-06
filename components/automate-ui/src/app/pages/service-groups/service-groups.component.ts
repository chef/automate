import { map, takeUntil, withLatestFrom, distinctUntilChanged } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subject, Observable, combineLatest } from 'rxjs';
import { HttpErrorResponse } from '@angular/common/http';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';
import {
  Chicklet,
  RollupServiceStatus,
  SearchBarCategoryItem,
  SortDirection
} from 'app/types/types';
import { EntityStatus } from 'app/entities/entities';
import {
  GetServiceGroupsSuggestions, UpdateServiceGroupsFilters, UpdateSelectedSG
} from 'app/entities/service-groups/service-groups.actions';
import {
  ServiceGroup,
  ServiceGroupsFilters,
  ServiceGroupsSuggestions,
  FieldDirection,
  ServiceGroupsHealthSummary,
  GroupServicesFilters
} from '../../entities/service-groups/service-groups.model';
import {
  serviceGroupsStatus,
  serviceGroupsList,
  serviceGroupsState,
  serviceGroupsHealth,
  serviceGroupsError
} from '../../entities/service-groups/service-groups.selector';
import { find, filter as fpFilter, pickBy, some, includes, get } from 'lodash/fp';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-service-groups',
  templateUrl: './service-groups.component.html',
  styleUrls: ['./service-groups.component.scss']
})

export class ServiceGroupsComponent implements OnInit, OnDestroy {
  public serviceGroupsList$: Observable<ServiceGroup[]>;
  public serviceGroupsStatus$: Observable<EntityStatus>;
  public serviceGroupsError$: Observable<HttpErrorResponse>;
  public sgHealthSummary: ServiceGroupsHealthSummary;

  // The selected service-group id that will be sent to the services-sidebar
  public selectedServiceGroupId: string;

  // The current page the user is visualizing
  public currentPage = 1;

  // The number of service groups to display per page
  public pageSize = 25;

  // Total number of service groups
  public totalServiceGroups = 0;

  // The currently selected health status filter
  public selectedStatus = 'total';
  private selectedStatus$: Observable<string>;

  // The collection of allowable status
  private allowedStatus = ['ok', 'critical', 'warning', 'unknown'];

  // Has this component been destroyed
  private isDestroyed: Subject<boolean> = new Subject();

  // The collection of allowable sort directions
  private allowedSortDirections = ['asc', 'desc', 'ASC', 'DESC'];

  // Sort field by default
  readonly defaultSortField = 'percent_ok';

  // Should the URL share dropdown be displayed
  shareDropdownVisible = false;

  // Should the search bar filter be displayed
  filtersVisible = true;

  // autocomplete suggestions
  serviceGroupsSuggestions$: Observable<string[]>;

  // The categories allowed for searching
  categoryTypes: SearchBarCategoryItem[] = [
    {
      type: 'origin',
      text: 'Origin',
      allowWildcards: true
    },
    {
      type: 'service_name',
      text: 'Service Name',
      allowWildcards: true
    },
    {
      type: 'version',
      text: 'Version',
      allowWildcards: true
    },
    {
      type: 'channel',
      text: 'Channel',
      allowWildcards: true
    },
    {
      type: 'application',
      text: 'Application',
      allowWildcards: true
    },
    {
      type: 'environment',
      text: 'Environment',
      allowWildcards: true
    },
    {
      type: 'site',
      text: 'Site',
      allowWildcards: true
    },
    {
      type: 'buildstamp',
      text: 'Build Timestamp',
      allowWildcards: true
    },
    {
      type: 'group_name',
      text: 'Group Name',
      allowWildcards: true
    }
  ];

  // The currently set collection of searchbar filters
  searchBarFilters$: Observable<Chicklet[]>;

  // The number of currently set searchbar filters
  numberOfSearchBarFilters$: Observable<number>;

  // The current number of failed nodes with searchbar filters
  failNodeCount$: Observable<number>;

  // The total number of nodes with searchbar filters
  totalNodeCount$: Observable<number>;

  // The current number of successful nodes with searchbar filters
  successNodeCount$: Observable<number>;

  // The current number of missing nodes with searchbar filters
  missingNodeCount$: Observable<number>;

  // The current number of nodes with searchbar and status filters
  totalNumberOfNodesWithStatusFilter$: Observable<number>;

  private selectedFieldDirection$: Observable<SortDirection>;
  private selectedSortField$: Observable<string>;
  private healthSummary$: Observable<ServiceGroupsHealthSummary>;
  private currentPage$: Observable<number>;
  public currentFieldDirection: SortDirection;
  public currentSortField: string;
  private defaultFieldDirection: FieldDirection = {
    name: 'ASC',
    percent_ok: 'ASC',
    environment: 'ASC',
    app_name: 'ASC'
  };

  constructor(
    private route: ActivatedRoute,
    public router: Router,
    public store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService
  ) { }

  ngOnInit() {
    const allUrlParameters$ = this.getAllUrlParameters();

    // URL change listener
    allUrlParameters$.pipe(takeUntil(this.isDestroyed)).subscribe(
      allUrlParameters => this.updateAllFilters(allUrlParameters));

    this.route.queryParamMap.pipe(
      distinctUntilChanged((a, b) => {
        return a.get('status') === b.get('status') &&
               a.get('page') === b.get('page') &&
               a.get('sortField') === b.get('sortField') &&
               a.get('sortDirection') === b.get('sortDirection');
      }),
      takeUntil(this.isDestroyed)
    ).subscribe(queryParams => this.listParamsChange(queryParams));

    combineLatest([
      this.route.queryParamMap.pipe(
        distinctUntilChanged((a, b) => {
          return a.get('sgId') === b.get('sgId') &&
                 a.get('sgStatus') === b.get('sgStatus') &&
                 a.get('sgPage') === b.get('sgPage');
        })
      ),
      this.store.select(serviceGroupsList)
    ])
    .pipe(takeUntil(this.isDestroyed))
    .subscribe(([queryParams]) => this.detailParamsChange(queryParams));

    this.serviceGroupsStatus$ = this.store.select(serviceGroupsStatus);
    this.serviceGroupsError$ = this.store.select(serviceGroupsError);
    this.serviceGroupsList$ = this.store.select(serviceGroupsList);
    this.serviceGroupsList$.pipe(
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

    this.selectedStatus$ = this.store.select(createSelector(serviceGroupsState,
      (serviceGroups) => serviceGroups.filters.status));
    this.selectedStatus$.pipe(takeUntil(this.isDestroyed)).subscribe((status) => {
      // This code enables pagination of service groups correctly, when the user selects
      // a Health Filter, we adjust the total number of service groups
      if ( includes(status, this.allowedStatus) ) {
          this.selectedStatus = status;
          this.totalServiceGroups = get(status, this.sgHealthSummary);
      } else {
          this.selectedStatus = 'total';
          this.totalServiceGroups = get('total', this.sgHealthSummary);
      }
      this.telemetryService.track('applicationsServiceGroupCount', {
        totalServiceGroups: this.totalServiceGroups,
        statusFilter: status
      });
    });

    this.healthSummary$ = this.store.select(serviceGroupsHealth);
    this.healthSummary$.pipe(takeUntil(this.isDestroyed))
      .subscribe((sgHealthSummary) => {
        this.sgHealthSummary = sgHealthSummary;
        // On first load or any health summary change, we update the total number of service groups
        this.totalServiceGroups = get(this.selectedStatus, this.sgHealthSummary);
      });


    this.selectedFieldDirection$ = this.store.select(createSelector(serviceGroupsState,
      (serviceGroups) => serviceGroups.filters.sortDirection));

    this.selectedFieldDirection$.pipe(takeUntil(this.isDestroyed))
      .subscribe(currentFieldDirection => this.currentFieldDirection = currentFieldDirection);

    this.selectedSortField$ = this.store.select(createSelector(serviceGroupsState,
      (serviceGroups) => serviceGroups.filters.sortField));

    this.selectedSortField$.pipe(takeUntil(this.isDestroyed)).subscribe(currentSortField =>
      this.currentSortField = currentSortField);

    this.currentPage$ = this.store.select(createSelector(serviceGroupsState,
      (serviceGroups) => serviceGroups.filters.page));

    this.currentPage$.pipe(takeUntil(this.isDestroyed))
      .subscribe(currentPage => this.currentPage = currentPage);

    this.serviceGroupsSuggestions$ = this.store.select(createSelector(serviceGroupsState,
      (serviceGroups) => serviceGroups.suggestions.values))
        .pipe(map((serviceGroupsSuggestions:
          ServiceGroupsSuggestions[]) => serviceGroupsSuggestions));

    this.searchBarFilters$ = allUrlParameters$.pipe(map((chicklets: Chicklet[]) =>
      chicklets.filter(chicklet => some({'type': chicklet.type}, this.categoryTypes))));

    this.numberOfSearchBarFilters$ = this.searchBarFilters$.pipe(
      map((chicklets: Chicklet[]) => chicklets.length));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  toggleFilters() {
    this.filtersVisible = !this.filtersVisible;
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
      const servicesFilters: GroupServicesFilters = {
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
    const searchBarFilters = fpFilter(chicklet => {
      return some({'type': chicklet.type}, this.categoryTypes);
    }, allParameters);

    const serviceGroupFilters: ServiceGroupsFilters = {
      status: status,
      sortField: sortField,
      sortDirection: sortDirection,
      page: pageField,
      pageSize: this.pageSize,
      searchBar: searchBarFilters
    };
    this.store.dispatch(new UpdateServiceGroupsFilters({filters: serviceGroupFilters}));
  }

  get shareUrl(): string {
    return window.location.href;
  }

  toggleShareDropdown(): void {
    this.shareDropdownVisible = !this.shareDropdownVisible;
  }

  hideShareDropdown(): void {
    this.shareDropdownVisible = false;
  }

  onSuggestValues(event: CustomEvent): void {
    this.store.dispatch(new GetServiceGroupsSuggestions( event.detail ));
  }

  onFilterAdded(event: CustomEvent): void {
    const {type, text} = event.detail;

    if (some({type}, this.categoryTypes) ) {
      const {queryParamMap} = this.route.snapshot;
      const queryParams = {...this.route.snapshot.queryParams};
      const values = queryParamMap.getAll(type).filter(value => value !== text).concat(text);

      queryParams[type] = values;

      delete queryParams['page'];

      this.router.navigate([], {queryParams});
    }
  }

  onFilterRemoved(event: CustomEvent): void {
    const {type, text} = event.detail;
    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const values = queryParamMap.getAll(type).filter(value => value !== text);

    if (values.length === 0) {
      delete queryParams[type];
    } else {
      queryParams[type] = values;
    }

    delete queryParams['page'];

    this.router.navigate([], {queryParams});
  }

  onFiltersClear(_event: CustomEvent): void {
    const queryParams = {...this.route.snapshot.queryParams};

    const filteredParams = pickBy((_value, key) => {
        return !some({'type': key}, this.categoryTypes);
      }, queryParams);

    delete filteredParams['page'];

    this.router.navigate([], {queryParams: filteredParams});
  }

  public statusFilter(status: string): void {
    const queryParams = {...this.route.snapshot.queryParams};
    if ( includes(status, this.allowedStatus) ) {
      queryParams['status'] = [status];
      this.telemetryService.track('applicationsStatusFilter',
        { entity: 'serviceGroup', statusFilter: status});
    } else {
      delete queryParams['status'];
    }

    delete queryParams['page'];
    delete queryParams['sgId'];
    delete queryParams['sgPage'];
    delete queryParams['sgStatus'];

    this.router.navigate([], {queryParams});
  }

  public onServiceGroupSelect(event: Event, id: string): void {
    event.preventDefault();

    const queryParams = { ...this.route.snapshot.queryParams };
    queryParams['sgId'] = id;
    delete queryParams['sgPage'];
    delete queryParams['sgStatus'];
    this.router.navigate([], { queryParams });
  }

  public updateServicesSidebar(servicesFilters: GroupServicesFilters): void {
    this.selectedServiceGroupId = servicesFilters.service_group_id;
    this.store.dispatch(new UpdateSelectedSG(servicesFilters));
    document.querySelector<HTMLElement>('app-services-sidebar').focus();
  }

  // TODO @afiune: Add links when they work
  public tooltipMessageFor(field: string): string {
    switch (field) {
      case 'env':
        return 'Add environment data. Learn more in Configuring the Habitat Supervisor.';
      case 'app':
        return 'Add application data. Learn more in Configuring the Habitat Supervisor.';
      default:
        return '--';
    }
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

  onPageChange(pageNumber: number): void {
    const queryParams = { ...this.route.snapshot.queryParams, page: pageNumber };
    const totalPages = Math.ceil(this.totalServiceGroups / this.pageSize) || 1;
    this.telemetryService.track('applicationsPageChange',
     { entity: 'serviceGroup', pageNumber: pageNumber, totalPages: totalPages});
    if (pageNumber <= 1) {
      delete queryParams['page'];
    }

    delete queryParams['sgId'];
    delete queryParams['sgPage'];
    delete queryParams['sgStatus'];

    this.router.navigate([], { queryParams });
  }

  onToggleSort(field: string): void {
    if (this.currentSortField === field) {
      const fieldDirection = this.currentFieldDirection === 'ASC' ? 'DESC' : 'ASC';
      this.onUpdateSort({field: field, fieldDirection: fieldDirection});
    } else {
      this.onUpdateSort({field: field, fieldDirection: this.defaultFieldDirection[field]});
    }
  }

  onUpdateSort(event): void {
    const {field, fieldDirection} = event;
    this.telemetryService.track('applicationsSort',
      { field: field, fieldDirection: fieldDirection});
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
    return this.defaultSortField;
  }

  private getSortDirection(allUrlParameters: Chicklet[]): 'ASC' | 'DESC' {
    const sortDirection = find( (chicklet) => {
        return chicklet.type === 'sortDirection';
      }, allUrlParameters);

    return sortDirection !== undefined && sortDirection.text === 'DESC' ? 'DESC' : 'ASC';
  }

  private getAllUrlParameters(): Observable<Chicklet[]> {
    return this.route.queryParamMap.pipe(map((params: ParamMap) => {
      return params.keys.reduce((list, key) => {
        const paramValues = params.getAll(key);
        return list.concat(paramValues.map(value => ({type: key, text: value})));
      }, []);
    }));
  }
}
