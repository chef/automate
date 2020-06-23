import { Component, Input } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';

@Component({
  selector: 'app-card',
  templateUrl: './card.component.html',
  styleUrls: ['./card.component.scss']
})
export class CardComponent {

  @Input() contentItem: ContentItem;

  constructor() { }
}
