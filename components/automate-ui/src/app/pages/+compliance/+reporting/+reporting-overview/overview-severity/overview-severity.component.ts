import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-overview-severity',
  templateUrl: './overview-severity.component.html',
  styleUrls: ['./overview-severity.component.scss']
})
export class OverviewSeverityComponent {

  @Input() data: any = {};

  styleForStatus(count: number) {
    const total = this.data['failed'];
    const percentage = total > 0 ? (count / total) * 100 : 0;
    return { width: `${percentage}%` };
  }

}
