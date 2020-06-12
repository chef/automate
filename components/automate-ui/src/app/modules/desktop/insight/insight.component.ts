import { Component, HostBinding, Input, Output, OnInit, EventEmitter } from '@angular/core';
import { Store, createSelector } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Desktop, TermFilter } from 'app/entities/desktop/desktop.model';
import { FilterUpdate } from 'app/page-components/insight-attributes-dropdown/insight-attributes-dropdown.model';
import { finalize } from 'rxjs/operators';
import * as moment from 'moment/moment';
import { saveAs } from 'file-saver';
import { DateTime } from 'app/helpers/datetime/datetime';
import { ClientRunsRequests } from 'app/entities/client-runs/client-runs.requests';
import { clientRunsState } from 'app/entities/client-runs/client-runs.selectors';
import { NodeFilter } from 'app/entities/client-runs/client-runs.model';

@Component({
  selector: 'app-insight',
  templateUrl: './insight.component.html',
  styleUrls: ['./insight.component.scss']
})
export class InsightComponent implements OnInit {

  @Input() desktops: Desktop[];
  @Input() selectedDesktop: Desktop;
  @Input() currentPage: number;
  @Input() pageSize: number;
  @Input() totalDesktops: number;
  @Input() termFilters: TermFilter[];
  @Input() @HostBinding('class.fullscreened') fullscreened = false;

  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() fullscreenToggled: EventEmitter<any> = new EventEmitter();
  @Output() pageChange: EventEmitter<number> = new EventEmitter();
  @Output() pageSizeChange: EventEmitter<number> = new EventEmitter();
  @Output() termFilterSelected: EventEmitter<TermFilter> = new EventEmitter();
  // Returns 'name', 'check-in', or 'platform'
  @Output() sortChange: EventEmitter<string> = new EventEmitter();
  @Output() desktopSelected: EventEmitter<Desktop> = new EventEmitter();

  public attributesMenuOpen = false;
  public downloadOptsVisible = false;
  public downloadInProgress = false;
  public downloadFailed = false;

  private nodeFilter: NodeFilter;

  constructor(
    private store: Store<NgrxStateAtom>,
    private requests: ClientRunsRequests
  ) { }

  ngOnInit() {
    this.store.select(createSelector(clientRunsState, (state) => state.nodeFilter)).subscribe(
      (nodeFilter: NodeFilter) => {
        this.nodeFilter = nodeFilter;
      });
  }

  public close(): void {
    this.closed.emit();
  }

  public onPageChange(pageNumber) {
    this.pageChange.emit(pageNumber);
  }

  public onPageSizeChanged(pageSize: number) {
    this.pageSizeChange.emit(pageSize);
  }

  public termFilterClicked(term: TermFilter): void {
    this.termFilterSelected.emit(term);
  }

  public sortOn(fieldName: string): void {
    this.sortChange.emit(fieldName);
  }

  public toggleAttributeMenu(): void {
    this.attributesMenuOpen = !this.attributesMenuOpen;
  }

  public updateFilters(event: FilterUpdate) {
    console.log(event);
  }

  toggleDownloadDropdown() {
    this.downloadOptsVisible = !this.downloadOptsVisible;
  }

  hideDownloadDropdown() {
    this.downloadOptsVisible = false;
  }

  onDownloadOptPressed(format) {
    this.downloadOptsVisible = false;

    const filename = `${moment().utc().format(DateTime.REPORT_DATE_TIME)}.${format}`;

    const onComplete = () => this.downloadInProgress = false;
    const onError = _e => this.downloadFailed = true;
    const types = {'json': 'application/json', 'csv': 'text/csv'};
    const onNext = data => {
      const type = types[format];
      const blob = new Blob([data], {type});
      saveAs(blob, filename);
    };

    this.requests.downloadNodes(format, this.nodeFilter).pipe(
      finalize(onComplete))
      .subscribe(onNext, onError);
  }
}
