import { map, takeUntil, finalize } from 'rxjs/operators';
import { HttpErrorResponse } from '@angular/common/http';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';
import { Subject, Observable, combineLatest } from 'rxjs';
import { Chicklet, NodeCount, RollupState, SortDirection } from '../../types/types';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { find, filter as fpFilter, pickBy, some, includes } from 'lodash/fp';
import {
  clientRunsLoading,
  clientRunsNodes,
  clientRunsState,
  clientRunsColumns
} from '../../entities/client-runs/client-runs.selectors';
import {
  Node,
  NodeFilter,
  FieldDirection,
  ColumnsPreference
} from '../../entities/client-runs/client-runs.model';
import {
  UpdateNodeFilters, GetWorkflowEnabled, GetNodeSuggestions, DeleteNodes, UpdateColumns
} from '../../entities/client-runs/client-runs.actions';
import { TelemetryService } from '../../services/telemetry/telemetry.service';
import * as moment from 'moment';
import { saveAs } from 'file-saver';
import { AuthorizedChecker } from 'app/helpers/auth/authorized';
import {
  ClientRunsRequests
} from '../../entities/client-runs/client-runs.requests';
import { EntityStatus } from '../../entities/entities';

@Component({
  selector: 'app-client-runs',
  templateUrl: './client-runs.component.html',
  styleUrls: ['./client-runs.component.scss']
})
export class ClientRunsComponent implements OnInit, OnDestroy {

  // Should the URL share dropdown be displayed
  shareDropdownVisible = false;

  // Should the search bar filter bar be displayed
  filtersVisible = true;

  // The number of nodes to display per page
  pageSize = 100;

  // The catagories allowed for searching
  categoryTypes: Chicklet[] = [
    {
      type: 'attribute',
      text: 'Attribute'
    },
    {
      type: 'chef_version',
      text: 'Chef Client Version'
    },
    {
      type: 'chef_tags',
      text: 'Chef Tag'
    },
    {
      type: 'cookbook',
      text: 'Cookbook'
    },
    {
      type: 'environment',
      text: 'Environment'
    },
    {
      type: 'error',
      text: 'Error'
    },
    {
      type: 'name',
      text: 'Node Name'
    },
    {
      type: 'platform',
      text: 'Platform'
    },
    {
      type: 'policy_group',
      text: 'Policy Group'
    },
    {
      type: 'policy_name',
      text: 'Policy Name'
    },
    {
      type: 'policy_revision',
      text: 'Policy Revision'
    },
    {
      type: 'recipe',
      text: 'Recipe'
    },
    {
      type: 'resource_name',
      text: 'Resource Name'
    },
    {
      type: 'role',
      text: 'Role'
    }
  ];

  // The default sort direction of the fields or columns
  defaultFieldDirection: FieldDirection = {
    name: 'ASC',
    checkin: 'DESC',
    uptime_seconds: 'DESC',
    platform: 'ASC',
    environment: 'ASC',
    policy_group: 'ASC',
    chef_version: 'ASC',
    deprecations_count: 'ASC'
  };

  // The currently selected node status filter
  selectedStatus$: Observable<string>;

  // The number of success, failure, and missing nodes with the current filters
  nodeCounts$: Observable<NodeCount>;

  // Does the request have the correct permissions
  permissionDenied$: Observable<boolean>;

  // Is the Error banner visible
  notificationVisible = false;

  // autocomplete suggestions
  nodeSuggestions$: Observable<any[]>;

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

  // Is the missing status currently selected
  isMissingStatusSelected$: Observable<boolean>;

  // Is the success status currently selected
  isSuccessStatusSelected$: Observable<boolean>;

  // Is the failure status currently selected
  isFailureStatusSelected$: Observable<boolean>;

  // Is the total or no status currently selected
  isTotalStatusSelected$: Observable<boolean>;

  // The currently selected page
  currentPage$: Observable<number>;

  // The currently viewable nodes
  nodes$: Observable<Node[]>;

  // Is workflow installed
  isWorkflowEnabled$: Observable<boolean>;

  // The direction nodes are sorted
  fieldDirection$: Observable<SortDirection>;

  // The field or column used to sort nodes
  sortField$: Observable<string>;

  // Editable columns in the table dropdown
  columns$: Observable<ColumnsPreference>;

  nodeFilter: NodeFilter;

  // Loading status of the data in the table
  loadedStatus$: Observable<EntityStatus>;

  // Used to check is the user is authorized
  authorizedChecker: AuthorizedChecker;

  // Has this component been destroyed
  private isDestroyed: Subject<boolean> = new Subject();

  // The collection of allowable status
  private allowedStatus = ['success', 'failure', 'missing'];

  // The collection of allowable sort directions
  private allowedSortDirections = ['asc', 'desc', 'ASC', 'DESC'];

  downloadOptsVisible = false;
  downloadInProgress = false;
  downloadFailed = false;
  downloadStatusVisible = false;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private store: Store<NgrxStateAtom>,
    private telemetryService: TelemetryService,
    private requests: ClientRunsRequests
  ) { }

  ngOnInit() {
    // Only load when first opening the /chef-runs page
    this.store.dispatch(new GetWorkflowEnabled());

    const allUrlParameters$ = this.getAllUrlParameters();

    this.searchBarFilters$ = allUrlParameters$.pipe(map((chicklets: Chicklet[]) =>
      chicklets.filter(chicklet => some({'type': chicklet.type}, this.categoryTypes))));

    this.numberOfSearchBarFilters$ = this.searchBarFilters$.pipe(
      map((chicklets: Chicklet[]) => chicklets.length));

    // URL change listener
    allUrlParameters$.pipe(takeUntil(this.isDestroyed)).subscribe(
      allUrlParameters => this.updateNodeFilters(allUrlParameters));

    this.nodes$ = this.store.select(clientRunsNodes);

    this.nodeCounts$ = this.store.select(createSelector(
      clientRunsState, (state) => state.nodeCount));
    this.failNodeCount$ = this.nodeCounts$.pipe(map(nodeCount => nodeCount.failure));
    this.totalNodeCount$ = this.nodeCounts$.pipe(map(nodeCount => nodeCount.total));
    this.successNodeCount$ = this.nodeCounts$.pipe(map(nodeCount => nodeCount.success));
    this.missingNodeCount$ = this.nodeCounts$.pipe(map(nodeCount => nodeCount.missing));

    this.fieldDirection$ = this.store.select(createSelector(clientRunsState,
      (state) => state.nodeFilter.sortDirection));

    this.sortField$ = this.store.select(createSelector(clientRunsState,
      (state) => state.nodeFilter.sortField));

    this.currentPage$ = this.store.select(createSelector(clientRunsState,
      (state) => state.nodeFilter.page));

    this.selectedStatus$ = this.store.select(createSelector(clientRunsState,
      (state) => state.nodeFilter.status));

    this.totalNumberOfNodesWithStatusFilter$ = combineLatest(
      this.selectedStatus$, this.nodeCounts$)
      .pipe(
        map(([status, nodeCount]) => {
          switch (status) {
            case 'success':
              return nodeCount.success;
            case 'failure':
              return nodeCount.failure;
            case 'missing':
              return nodeCount.missing;
            default:
              return nodeCount.total;
          }
        }));

    this.isMissingStatusSelected$ = this.selectedStatus$.pipe(map(status => status === 'missing'));
    this.isSuccessStatusSelected$ = this.selectedStatus$.pipe(map(status => status === 'success'));
    this.isFailureStatusSelected$ = this.selectedStatus$.pipe(map(status => status === 'failure'));
    this.isTotalStatusSelected$ = this.selectedStatus$.pipe(map(status => status === undefined));

    this.nodeSuggestions$ = this.store.select(createSelector(clientRunsState,
      (state) => state.nodeSuggestions)).pipe(map((nodeSuggestions: any[]) =>
      nodeSuggestions.map(item => item.text)));

    this.permissionDenied$ = this.store.select(createSelector(clientRunsState,
      (state) => state.errorResp)).pipe(map((httpErrorResponse: HttpErrorResponse) =>
        httpErrorResponse !== null && httpErrorResponse.status === 403
      ));

    this.store.select(createSelector(clientRunsState,
      (state) => state.errorResp)).pipe(map((httpErrorResponse: HttpErrorResponse) =>
        httpErrorResponse !== null && httpErrorResponse.status !== 403
      )).subscribe((isError: boolean) => {
        this.notificationVisible = isError;
      });

    this.isWorkflowEnabled$ = this.store.select(createSelector(clientRunsState,
      (state) => state.workflowEnabled));

    this.columns$ = this.store.select(clientRunsColumns);

    combineLatest(
      this.store.select(createSelector(
        state => state.sidebar, (state) => state.selectedOrgs)),
      this.store.select(createSelector(
        state => state.sidebar, (state) => state.selectedChefServers)),
      this.store.select(createSelector(
        clientRunsState, (state) => state.nodeFilter)))
      .pipe(
        map(([selectedOrgs, selectedChefServers, nodeFilter]) => {
          nodeFilter.organizations = selectedOrgs;
          nodeFilter.servers = selectedChefServers;
          return nodeFilter;
        }))
      .subscribe(
        (nodeFilter: NodeFilter) => {
          this.nodeFilter = nodeFilter;
        });

    this.loadedStatus$ = this.store.select(clientRunsLoading);

    this.authorizedChecker = new AuthorizedChecker(this.store);
    this.authorizedChecker.setPermissions([
      {
        endpoint: '/ingest/events/chef/node-multiple-deletes',
        paramList: [],
        verb: 'post'
      }
    ], []);
  }

  hideNotification() {
    this.notificationVisible = false;
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
    if (this.authorizedChecker) {
      this.authorizedChecker.destroy();
    }
  }

  toggleFilters() {
    this.filtersVisible = !this.filtersVisible;
  }

  toggleShareDropdown() {
    this.shareDropdownVisible = !this.shareDropdownVisible;
  }

  hideShareDropdown() {
    this.shareDropdownVisible = false;
  }


  onDeleteNodes(event): void {
    this.telemetryService.track('nodeDeletion', { count: event.nodeIds.length } );

    this.store.dispatch(new DeleteNodes( {nodeIdsToDelete: event.nodeIds} ));
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

  statusFilter(status) {
    const queryParams = {...this.route.snapshot.queryParams};
    if ( includes(status, this.allowedStatus) ) {
      queryParams['status'] = [status];
    } else {
      delete queryParams['status'];
    }

    delete queryParams['page'];

    this.router.navigate([], {queryParams});
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

  onUpdateColumns(columns: ColumnsPreference) {
    this.store.dispatch(new UpdateColumns(columns));
  }

  get shareUrl() {
    return window.location.href;
  }

  toggleDownloadDropdown() {
    this.downloadOptsVisible = !this.downloadOptsVisible;
  }

  hideDownloadDropdown() {
    this.downloadOptsVisible = false;
  }

  onDownloadOptPressed(format) {
    this.downloadOptsVisible = false;

    const filename = `${moment().format('YYYY-M-D')}.${format}`;

    const onComplete = () => this.downloadInProgress = false;
    const onError = _e => this.downloadFailed = true;
    const types = {'json': 'application/json', 'csv': 'text/csv'};
    const onNext = data => {
      const type = types[format];
      const blob = new Blob([data], {type});
      saveAs(blob, filename);
      this.hideDownloadStatus();
    };

    this.showDownloadStatus();
    this.requests.downloadNodes(format, this.nodeFilter).pipe(
      finalize(onComplete))
      .subscribe(onNext, onError);
  }

  showDownloadStatus() {
    this.downloadStatusVisible = true;
    this.downloadInProgress = true;
    this.downloadFailed = false;
  }

  hideDownloadStatus() {
    this.downloadStatusVisible = false;
    this.downloadInProgress = false;
    this.downloadFailed = false;
  }

  public loading(status: EntityStatus): boolean {
    return status === EntityStatus.loading;
  }

  public updateNodeFilters(allUrlParameters: Chicklet[]): void {
    const sortField = this.getSelectedSortField( allUrlParameters );

    const pageField = this.getSelectedPageNumber( allUrlParameters );

    const sortDirection = this.getSortDirection( allUrlParameters );

    const searchBarFilters = fpFilter(chicklet => {
        return some({'type': chicklet.type}, this.categoryTypes);
      }, allUrlParameters);

    const status = this.getSelectedStatus(allUrlParameters);

    const nodeFilters: NodeFilter = {
      page: pageField,
      pageSize: this.pageSize,
      searchBar: searchBarFilters,
      sortField: sortField,
      sortDirection: sortDirection,
      status: status
    };

    this.store.dispatch(new UpdateNodeFilters({filters: nodeFilters}));
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

  private getSelectedStatus(allUrlParameters: Chicklet[]): RollupState {
    const status = find((chicklet) => {
        return chicklet.type === 'status';
      }, allUrlParameters);

    if (status !== undefined && includes(status.text, this.allowedStatus)) {
      return status.text as RollupState;
    } else {
      return undefined;
    }
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
