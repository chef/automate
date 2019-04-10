import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-overview-status',
  templateUrl: './overview-status.component.html',
  styleUrls: ['./overview-status.component.scss']
})
export class OverviewStatusComponent {

  @Input() data: any = {};

  @Input() type: 'nodes' | 'controls';

}
