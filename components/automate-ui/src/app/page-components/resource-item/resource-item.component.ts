import { Component, Input } from '@angular/core';
import { NodeDetailsService } from '../../services/node-details/node-details.service';

@Component({
  selector: 'app-resource-item',
  templateUrl: './resource-item.component.html',
  styleUrls: ['./resource-item.component.scss']
})
export class ResourceItemComponent {
  @Input() resource: any;

  showDelta = false;

  constructor(private eventsService: NodeDetailsService) {}

  toggleDelta() {
    this.showDelta = !this.showDelta;
  }

  showModal() {
    this.eventsService.showModal(true);
  }
}

