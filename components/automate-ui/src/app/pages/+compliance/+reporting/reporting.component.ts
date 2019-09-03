import {
  debounceTime,
  distinctUntilChanged,
  finalize,
  map,
  switchMap,
  takeUntil,
  withLatestFrom
} from 'rxjs/operators';
import {
  Component,
  OnInit,
  OnDestroy
} from '@angular/core';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';
import { Subject, of as observableOf, Observable } from 'rxjs';
import * as moment from 'moment';
import {
  StatsService,
  SuggestionsService,
  ReportDataService,
  ReportQueryService,
  ReportQuery
} from '../shared/reporting';
import { saveAs } from 'file-saver';
import {
  Chicklet
} from '../../../types/types';
import { pickBy, some } from 'lodash/fp';

@Component({
  templateUrl: './reporting.component.html',
  styleUrls: ['./reporting.component.scss'],
  providers: [SuggestionsService]
})
export class ReportingComponent implements OnInit, OnDestroy {
  // Query search bar
  availableFilterTypes = [
    {
      'name': 'chef_server',
      'title': 'Chef Server',
      'description': '',
      'placeholder': 'Chef Server'
    },
    {
      'name': 'chef_tags',
      'title': 'Chef Tags',
      'description': '',
      'placeholder': 'Chef Tags'
    },
    {
      'name': 'control',
      'title': 'Control',
      'description': 'Add the title to filter this report against a control',
      'placeholder': 'Title'
    },
    {
      'name': 'environment',
      'title': 'Environment',
      'description': 'Add the environment name to filter this report to a specific environment',
      'placeholder': 'Environment'
    },
    {
      'name': 'inspec_version',
      'title': 'InSpec Version',
      'description': '',
      'placeholder': 'InSpec Version'
    },
    {
      'name': 'node',
      'title': 'Node Name',
      'description': 'Add the node name to filter this report against a specific node',
      'placeholder': 'Node Name'
    },
    {
      'name': 'organization',
      'title': 'Organization',
      'description': 'Add the organization to filter this report to a specific organization',
      'placeholder': 'Organization'
    },
    {
      'name': 'platform',
      'title': 'Platform',
      'description': 'Add the name to filter this report to a specific platform',
      'placeholder': 'Name'
    },
    {
      'name': 'policy_group',
      'title': 'Policy Group',
      'description': '',
      'placeholder': 'Policy Group'
    },
    {
      'name': 'policy_name',
      'title': 'Policy Name',
      'description': '',
      'placeholder': 'Policy Name'
    },
    {
      'name': 'profile',
      'title': 'Profile',
      'description': 'Add the name or ID to filter this report against a profile',
      'placeholder': 'Name or ID'
    },
    {
      'name': 'recipe',
      'title': 'Recipe',
      'description': 'Add the recipe to filter this report to a specific recipe',
      'placeholder': 'Recipe'
    },
    {
      'name': 'role',
      'title': 'Role',
      'description': 'Add the role to filter this report to a specific role',
      'placeholder': 'Role'
    }
  ];

  availableFilterValues = [];
  defaultFilterInputPlaceholder = 'Filter by...';
  inputSelectedFilter: {};
  removeSelectedFilter: {};

  downloadOptsVisible = false;
  shareOptsVisible = false;
  downloadList: Array<string> = [];
  downloadStatusVisible = false;
  downloadInProgress = false;
  downloadFailed = false;
  endDate$: Observable<Date>;

  showSummary = false;

  private suggestionSearchTerms = new Subject<{ 'type': string, 'text': string }>();

  // Used to notify all subscriptions to unsubscribe
  // http://stackoverflow.com/a/41177163/319074
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private router: Router,
    private statsService: StatsService,
    private suggestionsService: SuggestionsService,
    public reportQuery: ReportQueryService,
    public reportData: ReportDataService,
    private route: ActivatedRoute
  ) { }

  private getAllUrlParameters(): Observable<Chicklet[]> {
    return this.route.queryParamMap.pipe(map((params: ParamMap) => {
      return params.keys.reduce((list, key) => {
        const paramValues = params.getAll(key);
        return list.concat(paramValues.map(value => ({type: key, text: value})));
      }, []);
    }));
  }

  ngOnInit() {
    const allUrlParameters$ = this.getAllUrlParameters();

    this.endDate$ = this.reportQuery.state.pipe(map((reportQuery: ReportQuery) =>
      reportQuery.endDate));

    allUrlParameters$.pipe(takeUntil(this.isDestroyed)).subscribe(
      allUrlParameters => this.applyParamFilters(allUrlParameters));

    this.reportQuery.state.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(reportQuery => {
        this.reportData.nodesListParams.page = 1;
        this.reportData.profilesListParams.page = 1;
        this.getData(reportQuery);
      });

    this.suggestionSearchTerms.pipe(
      // wait 1/2 second after each keystroke before considering the term
      debounceTime(500),
      // ignore new term if same as previous term
      distinctUntilChanged(),
      // include currently selected report filters
      withLatestFrom(this.reportQuery.state),
      // switch to new search observable each time the term changes
      switchMap(([terms, reportQuery]) => {
        const { type, text } = terms;
        if (text && text.length > 0) {
          return this.getSuggestions(type, text, reportQuery);
        }
        return observableOf([]);
      }),
      takeUntil(this.isDestroyed)
    )
      .subscribe(suggestions => this.availableFilterValues = suggestions.filter(e => e.text));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getSuggestions(type, text, filters) {
    return this.suggestionsService.getSuggestions(type.replace('_id', ''), text, filters).pipe(
      map(data => {
        return data.map(item => {
          let title;
          // if the item has a version (as in the case of a profile), append
          // the version to the text so the user knows the version
          if (item.version) {
            title = `${item.text}, v${item.version}`;
          } else {
            title = item.text;
          }
          return Object.assign(item, { title: title });
        });
      }),
      takeUntil(this.isDestroyed)
    );
  }

  applyParamFilters(urlFilters: Chicklet[]) {
    const reportQuery = this.reportQuery.getReportQuery();
    reportQuery.filters = [];
    urlFilters.forEach( (filter: Chicklet) => {
      if (filter.type === 'end_time') {
        reportQuery.endDate = new Date(filter.text);
      } else if ( filter.type === 'start_time' ) {
        reportQuery.startDate = new Date(filter.text);
      } else if ( filter.type === 'date_interval' ) {
        reportQuery.interval = parseInt(filter.text, 10);
      } else {
        reportQuery.filters.push({ type: { name: filter.type }, value: { text: filter.text } });
      }
    });

    this.reportQuery.setState(reportQuery);
  }

  toggleDownloadDropdown() {
    this.downloadOptsVisible = !this.downloadOptsVisible;
  }

  toggleShareDropdown() {
    this.shareOptsVisible = !this.shareOptsVisible;
  }

  hideShareDropdown() {
    this.shareOptsVisible = false;
  }

  get shareUrl() {
    return window.location.href;
  }

  hideDownloadDropdown() {
    this.downloadOptsVisible = false;
  }

  onDownloadOptPressed(format) {
    this.downloadOptsVisible = false;

    const reportQuery = this.reportQuery.getReportQuery();
    const filename = `${moment(reportQuery.endDate).format('YYYY-M-D')}.${format}`;

    const onComplete = () => this.downloadInProgress = false;
    const onError = _e => this.downloadFailed = true;
    const types = { 'json': 'application/json', 'csv': 'text/csv' };
    const onNext = data => {
      const type = types[format];
      const blob = new Blob([data], { type });
      saveAs(blob, filename);
      this.hideDownloadStatus();
    };

    this.downloadList = [filename];
    this.showDownloadStatus();
    this.statsService.downloadReport(format, reportQuery).pipe(
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

  onEndDateChanged(event) {
    const endDate = event.detail;
    const startDate = this.reportQuery.findTimeIntervalStartDate(endDate);

    const queryParams = {...this.route.snapshot.queryParams};
    queryParams['start_time'] = moment(startDate).startOf('day').format();
    queryParams['end_time'] = moment(endDate).endOf('day').format();

    this.router.navigate([], {queryParams});
  }

  onSuggestValues(event) {
    const { type, text } = event.detail;
    this.suggestionSearchTerms.next({ 'type': type, 'text': text });
  }

  onFilterAdded(event) {
    const {type, value} = event.detail;

    let filterValue = value.text;
    let typeName = type.name;

    if (type.name === 'profile') {
      if ( value.id ) {
        typeName = 'profile_id';
        filterValue = value.id;
      } else {
        typeName = 'profile_name';
      }
    } else if (type.name === 'node') {
      if ( value.id ) {
        typeName = 'node_id';
        filterValue = value.id;
      } else {
        typeName = 'node_name';
      }
    } else if (type.name === 'control') {
      if ( value.id ) {
        typeName = 'control_id';
        filterValue = value.id;
      } else {
        typeName = 'control_name';
      }
    }

    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const existingValues = queryParamMap.getAll(typeName).filter(
      v => v !== filterValue).concat(filterValue);

    queryParams[typeName] = existingValues;

    this.router.navigate([], {queryParams});
  }

  onFilterRemoved(event) {
    const {type, value} = event.detail;

    const {queryParamMap} = this.route.snapshot;
    const queryParams = {...this.route.snapshot.queryParams};
    const values = queryParamMap.getAll(type.name).filter(v => v !== value.text);

    if (values.length === 0) {
      delete queryParams[type.name];
    } else {
      queryParams[type.name] = values;
    }

    this.router.navigate([], {queryParams});
  }

  onFiltersClear(_event) {
    const queryParams = {...this.route.snapshot.queryParams};

    const filteredParams = pickBy((_value, key) => {
        return !some({ 'name': key}, this.availableFilterTypes);
      }, queryParams);

    this.router.navigate([], {queryParams: filteredParams});
  }

  getData(reportQuery: ReportQuery) {
    if (reportQuery.filters.length === 0) { return; }
    this.reportData.getReportingSummary(reportQuery);
  }

  getSelectedFilters() {
    return this.reportQuery.getReportQuery().filters;
  }

  toggleSummary() {
    this.showSummary = !this.showSummary;
  }

  getIcon(status) {
    switch (status) {
      case 'failed': return 'report_problem';
      case 'passed': return 'check_circle';
      case 'skipped': return 'help';
      case 'unknown': return 'help';
    }
  }

  formatSummaryPhrase(status) {
    switch (status) {
      case 'failed': return 'Not Compliant';
      case 'passed': return 'Compliant';
      case 'skipped': return 'Skipped';
      case 'unknown': return 'Unknown';
    }
  }

  formatDuration(duration) {
    return moment.duration(duration).humanize();
  }

  formatDate(timestamp) {
    return moment(timestamp).format('MMMM Do[,] YYYY');
  }
}
