import {
  Component,
  Input,
  Output,
  EventEmitter
} from '@angular/core';
import { Chicklet } from '../../types/types';

@Component({
  selector: 'app-search-bar-filter-bar',
  templateUrl: './search-bar-filter-bar.component.html',
  styleUrls: ['./search-bar-filter-bar.component.scss']
})
export class SearchBarFilterBarComponent {
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
