import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Desktop, TermFilter } from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-insight',
  templateUrl: './insight.component.html',
  styleUrls: ['./insight.component.scss']
})
export class InsightComponent {

  @Input() visible = false;
  @Input() desktops: Desktop[];
  @Input() currentPage: number;
  @Input() pageSize: number;
  @Input() totalDesktops: number;
  @Input() termFilters: TermFilter[];

  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() pageChange: EventEmitter<number> = new EventEmitter();
  @Output() termFilterSelected: EventEmitter<TermFilter> = new EventEmitter();

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
}
