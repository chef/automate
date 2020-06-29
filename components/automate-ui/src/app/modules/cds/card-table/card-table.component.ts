import { Component, Input, OnChanges, SimpleChanges, Output, EventEmitter } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';
import { reduce, first, tail, concat, isEmpty, reverse } from 'lodash/fp';

@Component({
  selector: 'app-card-table',
  templateUrl: './card-table.component.html',
  styleUrls: ['./card-table.component.scss']
})
export class CardTableComponent implements OnChanges {

  @Input() contentItems: ContentItem[];
  @Output() onInstallItem: EventEmitter<string> = new EventEmitter();
  @Output() onDownloadItem: EventEmitter<ContentItem> = new EventEmitter();

  collectionOfPairs: ContentItem[][];

  constructor() { }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['contentItems']) {
     const paired = reduce((acc, item: ContentItem) => {
        if ( isEmpty(acc) ) {
          return [[item]];
        }
        const head: ContentItem[]  = first(acc);
        if ( head.length === 2 ) {
          return concat([[item]], acc);
        } else {
          return concat(tail(acc), [concat(head, item)]);
        }
      }, [], changes.contentItems.currentValue);

      this.collectionOfPairs = reverse(paired);
    }
  }

  installContentItem(item: ContentItem) {
    this.onInstallItem.emit(item.id);
  }

  downloadContentItem(item: ContentItem) {
    this.onDownloadItem.emit(item);
  }
}
