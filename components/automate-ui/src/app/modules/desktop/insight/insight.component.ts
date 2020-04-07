import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Node } from 'app/entities/client-runs/client-runs.model';

@Component({
  selector: 'app-insight',
  templateUrl: './insight.component.html',
  styleUrls: ['./insight.component.scss']
})
export class InsightComponent {

  @Input() visible = false;
  @Input() nodes: Node[];
  @Input() currentPage: number;
  @Input() pageSize: number;
  @Input() totalNodes: number;

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
