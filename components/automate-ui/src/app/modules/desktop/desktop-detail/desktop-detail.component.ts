import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, TermFilter } from 'app/entities/desktop/desktop.model';
import { DateTime } from 'app/helpers/datetime/datetime';

const historyTypes = [
  { text: 'converged', icon: 'check_box' },
  { text: 'unchanged', icon: 'indeterminate_check_box' },
  { text: 'error', icon: 'error' },
  { text: 'unknown', icon: 'help' }
];

const checkinHistory = Array.from(new Array(29)).map((_v, i) => {
  const historyType = historyTypes[Math.floor(Math.random() * historyTypes.length)];
  const date = new Date();
  return { ...historyType, date: new Date(date.setDate(date.getDate() - i)) };
});

console.log(checkinHistory);

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

  get checkinHistory() { return checkinHistory.slice(0, this.checkinNumDays + 1); }

  public ceil = Math.ceil;
  public DateTime = DateTime;
  public showCheckinDebug = false;
  public checkinTableType = 'grid';
  public checkinGridFlexType = 'wrap';
  public checkinNumDays = 14;

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
