import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';

import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  InstallContentItem
} from 'app/entities/cds/cds.actions';

@Component({
  selector: 'app-card',
  templateUrl: './card.component.html',
  styleUrls: ['./card.component.scss']
})
export class CardComponent implements OnChanges {

  @Input() contentItem: ContentItem;

  type: string;
  name: string;
  version: string;
  description: string;
  platforms: string[];
  canBeInstall: boolean;

  constructor(private store: Store<NgrxStateAtom>) { }

  clickedInstall(id: string) {
    this.store.dispatch(new InstallContentItem({id}));
  }

  clickedDownload(_id: string) {

  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['contentItem']) {
      this.type = changes.contentItem.currentValue.type;
      this.name = changes.contentItem.currentValue.name;
      this.version = changes.contentItem.currentValue.version;
      this.description = changes.contentItem.currentValue.description;
      this.platforms = changes.contentItem.currentValue.platforms;
      this.canBeInstall = changes.contentItem.currentValue.canBeInstall;
    }
  }
}
