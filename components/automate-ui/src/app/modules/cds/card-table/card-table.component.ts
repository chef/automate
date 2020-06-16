import { Component, Input } from '@angular/core';
import { ContentItem } from 'app/entities/cds/cds.model';

@Component({
  selector: 'app-card-table',
  templateUrl: './card-table.component.html',
  styleUrls: ['./card-table.component.scss']
})
export class CardTableComponent {

  @Input() contentItems: ContentItem[];

  
  constructor() { }
}
