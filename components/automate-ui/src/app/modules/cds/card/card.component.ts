import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';

import { CdsRequests } from 'app/entities/cds/cds.requests';

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

  constructor(private cdsRequests: CdsRequests) { }

  clickedInstall(id: string) {
    this.cdsRequests.installContentItem(id);
  }

  clickedDownload(id: string) {
    console.info('clickedDownload with ID: ' + id);
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
