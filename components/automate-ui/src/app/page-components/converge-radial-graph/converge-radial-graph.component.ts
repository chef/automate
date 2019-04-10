import { Component,
         Input,
         OnChanges } from '@angular/core';
import { isEmpty } from 'lodash';
import { NodeCount } from '../../types/types';

@Component({
  selector: 'app-converge-radial-graph',
  templateUrl: './converge-radial-graph.component.html',
  styleUrls: ['./converge-radial-graph.component.scss']
})

export class ConvergeRadialGraphComponent implements OnChanges {

  labelDefault = 'No Nodes Found';
  label = this.labelDefault;
  emptyCount: NodeCount = {success: 0, failure: 0, missing: 0, total: 0};
  @Input() count = this.emptyCount;
  firstCountRetrieval = true;

  ngOnChanges(changes): void {
    if (changes.count) {
      if (isEmpty(changes.count.currentValue)) {
        this.count = this.emptyCount;
      } else {
        this.count = changes.count.currentValue;
      }
      this.updateChartData();
    }
  }

  private updateChartData() {
    if (this.count.total === 0) {
      this.label = this.labelDefault;
    } else {
      this.label = this.count.total + ' Total Nodes';
    }
  }
}
