import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Desktop } from 'app/entities/desktop/desktop.model';

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

  @Output() closed: EventEmitter<any> = new EventEmitter();
  @Output() pageChange: EventEmitter<number> = new EventEmitter();

  constructor() { }

  public close(): void {
    this.closed.emit();
  }

  public onPageChange(pageNumber) {
    this.pageChange.emit(pageNumber);
  }
}
