import {
  debounceTime,
  distinctUntilChanged,
  filter,
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
import { ActivatedRoute, Router } from '@angular/router';
import { Subject, of as observableOf } from 'rxjs';
import * as moment from 'moment';
import {
  StatsService,
  SuggestionsService,
  ReportDataService,
  ReportQueryService
} from '../shared/reporting';
import { saveAs } from 'file-saver';

@Component({
  templateUrl: './reporting.component.html',
  styleUrls: ['./reporting.component.scss'],
  providers: [SuggestionsService]
})
export class ReportingComponent implements OnInit, OnDestroy {
  // Query search bar
  availableFilterTypes = [
    {
      'name': 'profile',
      'title': 'Profile',
      'description': 'Add the name or ID to filter this report against a profile',
      'placeholder': 'Name or ID'
    },
    {
      'name': 'node',
      'title': 'Node',
      'description':
        'Add the name, ID, or hostname to filter this report against a specific node',
      'placeholder': 'Name, ID, or hostname'
    },
    {
      'name': 'platform',
      'title': 'Platform',
      'description': 'Add the name to filter this report to a specific platform',
      'placeholder': 'Name'
    },
    {
      'name': 'environment',
      'title': 'Environment',
      'description': 'Add the name to filter this report to a specific environment',
      'placeholder': 'Name'
    },
    {
      'name': 'control',
      'title': 'Control',
      'description': 'Add the title to filter this report against a control',
      'placeholder': 'Title'
    },
    {
      'name': 'role',
      'title': 'Role',
      'description': 'Add the role to filter this report to a specific role',
      'placeholder': 'Role'
    },
    {
      'name': 'recipe',
      'title': 'Recipe',
      'description': 'Add the recipe to filter this report to a specific recipe',
      'placeholder': 'Recipe'
    }
  ];

  availableFilterValues = [];
  defaultFilterInputPlaceholder = 'Filter by...';
  inputSelectedFilter: {};
  removeSelectedFilter: {};

  downloadOptsVisible = false;
  downloadList: Array<string> = [];
  downloadStatusVisible = false;
  downloadInProgress = false;
  downloadFailed = false;

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
    route: ActivatedRoute
  ) {
    route.queryParams.pipe(
      takeUntil(this.isDestroyed),
      filter(params => params['filters']))
      .subscribe(params => {
        this.applyParamFilters(params['filters']);
      });
  }

  ngOnInit() {
    this.reportQuery.filters.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(filters => {
        this.reportData.nodesListParams.page = 1;
        this.reportData.profilesListParams.page = 1;
        this.getData(filters);
      });

    this.suggestionSearchTerms.pipe(
      // wait 1/2 second after each keystroke before considering the term
      debounceTime(500),
      // ignore new term if same as previous term
      distinctUntilChanged(),
      // include currently selected report filters
      withLatestFrom(this.reportQuery.filters),
      // switch to new search observable each time the term changes
      switchMap(([terms, filters]) => {
        const { type, text } = terms;
        if (text && text.length > 0) {
          return this.getSuggestions(type, text, filters);
        }
        return observableOf([]);
      }),
      takeUntil(this.isDestroyed)
    )
      .subscribe(suggestions => this.availableFilterValues = suggestions);
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

  applyParamFilters(filters) {
    this.reportQuery.clearFilters();
    const filterSets = filters.split('+');
    filterSets.forEach(element => {
      const arrayFilters = element.split(':');
      this.reportQuery.addFilter(
        { type: { name: arrayFilters[0] }, value: { text: arrayFilters[1] } }
      );
    });
  }

  toggleDownloadDropdown() {
    this.downloadOptsVisible = !this.downloadOptsVisible;
  }

  hideDownloadDropdown() {
    this.downloadOptsVisible = false;
  }

  onDownloadOptPressed(format) {
    this.downloadOptsVisible = false;

    const filters = this.reportQuery.filters.getValue();
    const filename = `${moment(this.reportQuery.endDate).format('YYYY-M-D')}.${format}`;

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
    this.statsService.downloadReport(format, filters).pipe(
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
    const { intervals, interval } = this.reportQuery;
    const endDate = event.detail;
    const startDate = intervals[interval][1](endDate);
    this.reportQuery.setDateRange(startDate, endDate);
  }

  onSuggestValues(event) {
    const { type, text } = event.detail;
    this.suggestionSearchTerms.next({ 'type': type, 'text': text });
  }

  onFilterAdded(event) {
    this.reportQuery.addFilter(event.detail);
  }

  onFilterRemoved(event) {
    this.router.navigate([], { queryParams: {} });
    this.reportQuery.removeFilter(event.detail);
  }

  onFiltersClear(_event) {
    this.reportQuery.clearFilters();
    this.router.navigate([], { queryParams: { filters: undefined } });
  }

  getData(filters) {
    if (filters.length === 0) { return; }
    this.reportData.getReportingSummary(filters);
  }

  getSelectedFilters() {
    return this.reportQuery.filters.getValue()
      .filter(f => !f['end_time'] && !f['start_time']);
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
