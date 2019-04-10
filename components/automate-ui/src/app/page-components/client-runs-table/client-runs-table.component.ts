import {
  Component,
  EventEmitter,
  Input,
  Output,
  OnInit,
  OnChanges,
  ChangeDetectionStrategy,
  SimpleChanges } from '@angular/core';

import {
  Node,
  FieldDirection,
  ColumnsPreference
} from '../../entities/client-runs/client-runs.model';

import { SortDirection } from '../../types/types';
@Component({
  selector: 'app-client-runs-table',
  templateUrl: './client-runs-table.component.html',
  styleUrls: ['./client-runs-table.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})

export class ClientRunsTableComponent implements OnInit, OnChanges {
  @Input() totalNodes;
  @Input() nodes: Node[] = [];
  @Input() currentPage = 1;
  @Input() pageSize = 100;
  @Input() selectedFieldDirection: SortDirection;
  @Input() selectedSortField: string;
  @Input() defaultFieldDirection: FieldDirection;
  @Input() columns: ColumnsPreference;
  @Input() loading: boolean;
  @Input() canDeleteNodes: boolean;
  @Output() deleteNodes: EventEmitter<any> = new EventEmitter<any>();
  @Output() updateSort: EventEmitter<any> = new EventEmitter<any>();
  @Output() pageChange: EventEmitter<any> = new EventEmitter<any>();
  @Output() updateColumns: EventEmitter<any> = new EventEmitter<any>();
  deletableNodes: Node[] = [];
  columnDropdownVisible = false;

  public displayPolicyNodes: boolean;
  public displayEnvironmentNodes: boolean;

  constructor() {}

  ngOnInit() {
    this.displayPolicyNodes = true;
    this.displayEnvironmentNodes = true;
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['nodes']) {
      this.nodes = changes['nodes'].currentValue;
      this.deletableNodes = this.nodes.filter((node: Node) => node.status === 'missing');
    }
  }

  onDeleteNodes(event): void {
    this.deleteNodes.emit(event);
  }

  onPageChange(pageNumber) {
    this.pageChange.emit(pageNumber);
  }

  toggleColumnDropdown() {
    this.columnDropdownVisible = !this.columnDropdownVisible;
  }

  closeColumnDropdown() {
    this.columnDropdownVisible = false;
  }

  updateColumnVisibility(key) {
    this.columns[key] = !this.columns[key];

    this.updateColumns.emit(this.columns);
  }

  sortIcon(field: string): string {
    if (field === this.selectedSortField) {
      return 'sort-' + this.selectedFieldDirection.toLowerCase();
    } else {
      return 'sort';
    }
  }

  onToggleSort(field: string) {
    if (this.selectedSortField === field) {
      const fieldDirection = this.selectedFieldDirection === 'ASC' ? 'DESC' : 'ASC';
      this.updateSort.emit({field: field, fieldDirection: fieldDirection});
    } else {
      this.updateSort.emit({field: field, fieldDirection: this.defaultFieldDirection[field]});
    }
  }
}
