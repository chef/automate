import { Component, Input, Output, OnChanges, SimpleChanges, EventEmitter } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';

@Component({
  selector: 'app-card',
  templateUrl: './card.component.html',
  styleUrls: ['./card.component.scss']
})
export class CardComponent implements OnChanges {

  @Input() contentItem: ContentItem;
  @Output() onInstallItem: EventEmitter<string> = new EventEmitter();
  @Output() onDownloadItem: EventEmitter<string> = new EventEmitter();

  type: string;
  name: string;
  version: string;
  description: string;
  platforms: string[];
  canBeInstall: boolean;

  constructor() { }

  clickedInstall(id: string) {
    this.onInstallItem.emit(id);
  }

  clickedDownload(id: string) {
    this.onDownloadItem.emit(id);
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
