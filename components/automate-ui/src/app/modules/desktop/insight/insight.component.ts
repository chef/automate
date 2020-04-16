import { Component, HostBinding, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, TermFilter } from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-insight',
  templateUrl: './insight.component.html',
  styleUrls: ['./insight.component.scss']
})
export class InsightComponent {

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
  @Output() termFilterSelected: EventEmitter<TermFilter> = new EventEmitter();
  // Returns 'name', 'check-in', or 'platform'
  @Output() sortChange: EventEmitter<string> = new EventEmitter();

  @Output() desktopSelected: EventEmitter<Desktop> = new EventEmitter();

  constructor() { }

  public close(): void {
    this.closed.emit();
  }

  public onPageChange(pageNumber) {
    this.pageChange.emit(pageNumber);
  }

  public termFilterClicked(term: TermFilter): void {
    this.termFilterSelected.emit(term);
  }

  public sortOn(fieldName: string): void {
    this.sortChange.emit(fieldName);
  }
}
