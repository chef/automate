import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';
import { reduce, first, tail, concat, isEmpty, reverse } from 'lodash/fp';

@Component({
  selector: 'app-card-table',
  templateUrl: './card-table.component.html',
  styleUrls: ['./card-table.component.scss']
})
export class CardTableComponent implements OnChanges {

  @Input() contentItems: ContentItem[];

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
      }, [], this.contentItems);

      this.collectionOfPairs = reverse(paired);
    }
  }
}
