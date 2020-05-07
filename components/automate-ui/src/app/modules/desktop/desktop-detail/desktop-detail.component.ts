import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, TermFilter } from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-desktop-detail',
  templateUrl: './desktop-detail.component.html',
  styleUrls: ['./desktop-detail.component.scss']
})
export class DesktopDetailComponent {

  @Input() desktop: Desktop;
  @Input() currentPage: number;
  @Input() pageSize: number;
  @Input() totalDesktops: number;
  @Input() termFilters: TermFilter[];
  @Input() fullscreened = false;

  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() fullscreenToggled: EventEmitter<void> = new EventEmitter();
  @Output() pageChange: EventEmitter<number> = new EventEmitter();
  @Output() termFilterSelected: EventEmitter<TermFilter> = new EventEmitter();
  // Returns 'name', 'check-in', or 'platform'
  @Output() sortChange: EventEmitter<string> = new EventEmitter();

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
