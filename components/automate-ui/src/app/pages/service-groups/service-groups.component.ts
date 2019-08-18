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
} from '../../types/types';
import { EntityStatus } from '../../entities/entities';
import {
  GetNodeSuggestions, UpdateServiceGroupFilters, UpdateSelectedSG
} from 'app/entities/service-groups/service-groups.actions';
import {
  ServiceGroup, ServiceGroupFilters, FieldDirection, HealthSummary, ServicesFilters
} from '../../entities/service-groups/service-groups.model';
import {
  serviceGroupStatus,
  allServiceGroups,
  serviceGroupState,
  allServiceGroupHealth,
  serviceGroupErrorResp
} from '../../entities/service-groups/service-groups.selector';
import { find, filter as fpFilter, pickBy, some, includes, get } from 'lodash/fp';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-service-groups',
  templateUrl: './service-groups.component.html',
  styleUrls: ['./service-groups.component.scss']
})

export class ServiceGroupsComponent implements OnInit, OnDestroy {
  public serviceGroups$: Observable<ServiceGroup[]>;
  public serviceGroupStatus$: Observable<EntityStatus>;
  public serviceGroupError$: Observable<HttpErrorResponse>;
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

  // Should the search bar filter bar be displayed
  filtersVisible = true;

  // autocomplete suggestions
  nodeSuggestions$: Observable<any[]>;

  // The catagories allowed for searching
  categoryTypes: SearchBarCategoryItem[] = [
    {
      type: 'origin',
      text: 'Origin',
      allowWildcards: true
    },
    {
      type: 'service',
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
      allowWildcards: false,
      providedValues: [
        {name: 'ipos', title: 'iPOS', icon: null},
        {name: 'ikds', title: 'iKDS', icon: null}
      ]
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
      type: 'group',
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
  private healthSummary$: Observable<HealthSummary>;
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
      this.store.select(allServiceGroups)
      ])
    .pipe(takeUntil(this.isDestroyed))
    .subscribe(([queryParams]) => this.detailParamsChange(queryParams));

    this.serviceGroupStatus$ = this.store.select(serviceGroupStatus);
    this.serviceGroupError$ = this.store.select(serviceGroupErrorResp);
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

    this.selectedStatus$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.filters.status));
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

    this.healthSummary$ = this.store.select(allServiceGroupHealth);
    this.healthSummary$.pipe(takeUntil(this.isDestroyed))
      .subscribe((sgHealthSummary) => {
        this.sgHealthSummary = sgHealthSummary;
        // On first load or any health summary change, we update the total number of service groups
        this.totalServiceGroups = get(this.selectedStatus, this.sgHealthSummary);
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

    this.nodeSuggestions$ = this.store.select(createSelector(serviceGroupState,
      (state) => state.nodeSuggestions)).pipe(map((nodeSuggestions: any[]) =>
      nodeSuggestions.map(item => item.text)));

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
    const searchBarFilters = fpFilter(chicklet => {
      return some({'type': chicklet.type}, this.categoryTypes);
    }, allParameters);

    const serviceGroupFilters: ServiceGroupFilters = {
      status: status,
      sortField: sortField,
      sortDirection: sortDirection,
      page: pageField,
      pageSize: this.pageSize,
      searchBar: searchBarFilters
    };
    this.store.dispatch(new UpdateServiceGroupFilters({filters: serviceGroupFilters}));
  }

  get shareUrl() {
    return window.location.href;
  }

  toggleShareDropdown() {
    this.shareDropdownVisible = !this.shareDropdownVisible;
  }

  hideShareDropdown() {
    this.shareDropdownVisible = false;
  }

  onSuggestValues(event) {
    this.store.dispatch(new GetNodeSuggestions( event.detail ));
  }

  onFilterAdded(event) {
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

  onFilterRemoved(event) {
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

  onFiltersClear(_event) {
    const queryParams = {...this.route.snapshot.queryParams};

    const filteredParams = pickBy((_value, key) => {
        return !some({'type': key}, this.categoryTypes);
      }, queryParams);

    delete filteredParams['page'];

    this.router.navigate([], {queryParams: filteredParams});
  }

  public statusFilter(status) {
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

  onPageChange(pageNumber: number) {
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
