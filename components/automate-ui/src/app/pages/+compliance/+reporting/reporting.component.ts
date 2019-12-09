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
import { Subject, Observable } from 'rxjs';
import * as moment from 'moment';
import {
  StatsService,
  SuggestionsService,
  ReportDataService,
  ReportQueryService,
  ReportQuery
} from '../shared/reporting';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { saveAs } from 'file-saver';
import {
  Chicklet
} from '../../../types/types';
import { DateTime } from 'app/helpers/datetime/datetime';
import { pickBy } from 'lodash/fp';
import { FilterC } from './types';

@Component({
  templateUrl: './reporting.component.html',
  styleUrls: ['./reporting.component.scss'],
  providers: [SuggestionsService]
})
export class ReportingComponent implements OnInit, OnDestroy {
  allowedURLFilterTypes = [
    'chef_server',
    'chef_tags',
    'control_id',
    'control_name',
    'control_tag_key',
    'environment',
    'inspec_version',
    'job_id',
    'node_id',
    'node_name',
    'organization',
    'platform_with_version',
    'policy_group',
    'policy_name',
    'profile_id',
    'profile_with_version',
    'profile_name',
    'recipe',
    'role'
  ];

  // Query search bar
  availableFilterTypes = [
    {
      'name': 'organization',
      'title': 'Chef Organization',
      'description': 'Add the organization to filter this report to a specific organization',
      'placeholder': 'Chef Organization'
    },
    {
      'name': 'chef_server',
      'title': 'Chef Server',
      'description': '',
      'placeholder': 'Chef Server'
    },
    {
      'name': 'chef_tags',
      'title': 'Chef Tag',
      'description': '',
      'placeholder': 'Chef Tag'
    },
    {
      'name': 'control_tag_key',
      'title': 'Control Tag',
      'description': '',
      'placeholder': 'Control Tag'
    },
    {
      'name': 'control',
      'title': 'Controls',
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
      'name': 'platform_with_version',
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
      'name': 'profile_with_version',
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

  private NO_VALUE = 'no value';
  private noValuesControlTagFilter = {
    id: '',
    score: 1,
    text: this.NO_VALUE,
    title: 'no value',
    version: ''
  };

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
  filters$: Observable<FilterC[]>;
  ChefDateTime = DateTime.CHEF_DATE_TIME;

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
    private route: ActivatedRoute,
    private layoutFacade: LayoutFacadeService
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
    this.layoutFacade.showSidebar('compliance');
    const allUrlParameters$ = this.getAllUrlParameters();

    this.endDate$ = this.reportQuery.state.pipe(map((reportQuery: ReportQuery) =>
      this.convertMomentToDateWithoutTimezone(reportQuery.endDate)));

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
        return this.getSuggestions(type, text, reportQuery);
      }),
      takeUntil(this.isDestroyed)
    ).subscribe(suggestions => this.availableFilterValues = suggestions.filter(e => e.text));

    this.filters$ = this.reportQuery.state.pipe(map((reportQuery: ReportQuery) =>
      reportQuery.filters.filter((filter) => filter.value.text !== undefined).map(filter => {
        filter.value.id = filter.value.text;
        if (['profile_id', 'node_id', 'control_id'].indexOf(filter.type.name) >= 0) {
          const name = this.reportQuery.getFilterTitle(filter.type.name, filter.value.id);
          if (name !== undefined) {
            filter.value.text = name;
          }
        }
        return filter;
      })));
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
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
    const filename = `${reportQuery.endDate.format('YYYY-M-D')}.${format}`;

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
    const queryParams = {...this.route.snapshot.queryParams};
    const endDate = moment.utc(event.detail);

    if (moment().utc().format('YYYY-MM-DD') === moment(endDate).format('YYYY-MM-DD')) {
      delete queryParams['end_time'];
    } else {
      queryParams['end_time'] = moment(endDate).format('YYYY-MM-DD');
    }

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

    if (type.name === 'profile_with_version') {
      if ( value.id ) {
        typeName = 'profile_id';
        filterValue = value.id;
        this.reportQuery.setFilterTitle(typeName, value.id, value.title);
      } else {
        typeName = 'profile_with_version';
      }
    } else if (type.name === 'node') {
      if ( value.id ) {
        typeName = 'node_id';
        filterValue = value.id;
        this.reportQuery.setFilterTitle(typeName, value.id, value.title);
      } else {
        typeName = 'node_name';
      }
    } else if (type.name === 'control') {
      if ( value.id ) {
        typeName = 'control_id';
        filterValue = value.id;
        this.reportQuery.setFilterTitle(typeName, value.id, value.title);
      } else {
        typeName = 'control_name';
      }
    } else if (this.isTypeControlTag(type.name)) {
      if (value.title === 'no value') {
        typeName = type.name;
        filterValue = value.id;
        this.reportQuery.setFilterTitle(typeName, value.id, value.title);
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
    const values = queryParamMap.getAll(type.name).filter(v => v !== value.id);

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

      // Only handles control_tag filters
      if (key.includes('control_tag')) {
        return false;
      }

        return this.allowedURLFilterTypes.indexOf(key) < 0;
      }, queryParams);


    this.router.navigate([], {queryParams: filteredParams});
  }

  getData(reportQuery: ReportQuery) {
    this.reportData.getReportingSummary(reportQuery);
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
    return moment.utc(timestamp).format('MMMM Do[,] YYYY');
  }

  getSuggestions(type: string, text: string, reportQuery: ReportQuery) {
    return this.suggestionsService.getSuggestions(type.replace('_id', ''), text, reportQuery).pipe(
      map(data => {

        // Adding the option of 'no value' only to the control tag third level suggestions
        if (type === 'control_tag_value') {
          data.unshift(this.noValuesControlTagFilter);
        }

        return data.map(item => Object.assign(item, { title: item.text }));
      }),
      takeUntil(this.isDestroyed)
    );
  }

  applyParamFilters(urlFilters: Chicklet[]) {
    const reportQuery = this.reportQuery.getReportQuery();

    reportQuery.filters = urlFilters.filter(
      (urlParm: Chicklet) => this.isTypeControlTag(urlParm.type)
        || this.allowedURLFilterTypes.indexOf(urlParm.type) >= 0)
      .map((urlParm: Chicklet) => {
        return { type: { name: urlParm.type }, value: { text: urlParm.text } };
      });

    reportQuery.interval = this.getDateInterval(urlFilters);
    reportQuery.endDate = this.getEndDate(urlFilters);
    reportQuery.startDate = this.reportQuery.findTimeIntervalStartDate(
      reportQuery.interval, reportQuery.endDate);

    this.reportQuery.setState(reportQuery);
  }

  isTypeControlTag(type: string) {
    return type.search('control_tag:') !== -1;
  }

  getEndDate(urlFilters: Chicklet[]): moment.Moment {
    const foundFilter = urlFilters.find( (filter: Chicklet) => filter.type === 'end_time');

    if (foundFilter !== undefined) {
      const endDate = moment.utc(foundFilter.text + 'GMT+00:00', 'YYYY-MM-DDZ');
      if (endDate.isValid()) {
        return endDate.utc().startOf('day').add(12, 'hours');
      } else {
        const queryParams = {...this.route.snapshot.queryParams};
        delete queryParams['end_time'];

        this.router.navigate([], {queryParams});
      }
    }
    return moment().utc().startOf('day').add(12, 'hours');
  }

  getDateInterval(urlFilters: Chicklet[]): number {
    const foundFilter = urlFilters.find( (filter: Chicklet) => filter.type === 'date_interval');

    if (foundFilter === undefined) {
      return 0;
    } else {
      const dateInterval = parseInt(foundFilter.text, 10);
      if ( !isNaN(dateInterval) && dateInterval >= 0 &&
        dateInterval < this.reportQuery.intervals.length ) {
        return dateInterval;
      } else {
        const queryParams = {...this.route.snapshot.queryParams};
        delete queryParams['date_interval'];

        this.router.navigate([], {queryParams});
      }
    }

    return 0;
  }

  convertMomentToDateWithoutTimezone(m: moment.Moment): Date {
    return new Date(m.year(), m.month(), m.date());
  }
}
