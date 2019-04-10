import {
  Component,
  Input,
  Output,
  EventEmitter
} from '@angular/core';
import { Chicklet } from '../../types/types';

@Component({
  selector: 'app-client-runs-search-filters',
  templateUrl: './client-runs-search-filters.component.html',
  styleUrls: ['./client-runs-search-filters.component.scss']
})
export class ClientRunsSearchFiltersComponent {
  @Input() filters: Chicklet[] = [];
  @Output() filtersCleared: EventEmitter<any> = new EventEmitter<any>();
  @Output() filterRemoved: EventEmitter<any> = new EventEmitter<any>();

  onClearClick(): void {
    this.filtersCleared.emit({});
  }

  onRemoveFilterClick(filter: any): void {
    this.filterRemoved.emit({ detail: filter});
  }
}
