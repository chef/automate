import {
  AfterContentInit,
  ChangeDetectionStrategy,
  Component,
  Input,
  OnChanges,
  OnInit,
  SimpleChange
} from '@angular/core';
import { NodeDetailsService } from '../../services/node-details/node-details.service';
import { NodeRun } from '../../types/types';
import { find } from 'lodash';

@Component({
  selector: 'app-resources',
  templateUrl: './resources.component.html',
  styleUrls: ['./resources.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class ResourcesComponent implements OnInit, AfterContentInit, OnChanges {

  @Input() nodeRun: NodeRun;

  currentPage = 1;
  pageSize = 50;
  showModal = false;
  showDiff = false;
  resourcesDataCollection = [];
  step = 0;
  active = '';
  resourcesTotal = 0;
  failedTotal = 0;
  successTotal = 0;
  unchangedTotal = 0;
  unprocessedTotal = 0;

  constructor(private eventService: NodeDetailsService) {}

  ngOnInit() {
    if (this.active === '') {
      this.filter('total');
    }
  }

  ngAfterContentInit() {
    this.resourcesDataCollection = this.nodeRun.resources;
    this.calculateTotals(this.resourcesDataCollection);
  }

  ngOnChanges(changes: {
    [propertyName: string]: SimpleChange
  }) {
    if (changes['nodeRun']) {
      this.resourcesDataCollection = changes['nodeRun'].currentValue.resources;

      this.calculateTotals(this.resourcesDataCollection);
    }
  }

  calculateTotals(resources) {
    if (resources) {
      this.resourcesTotal = resources.length;
      this.failedTotal = this.countStatusTypes(['failed'], resources);
      this.successTotal = this.countStatusTypes(['updated'], resources);
      this.unprocessedTotal = this.countStatusTypes(['unprocessed'], resources);
      this.unchangedTotal = this.countStatusTypes(['up-to-date', 'skipped'], resources);

      resources.forEach((d, i) => {
        d.step = `${i + 1}/${this.resourcesTotal}`;
      });
    }
  }

  countStatusTypes(statusTypes: string[], resources): number {
    return resources
      .filter((data) => find(statusTypes, (statusType) => statusType === data.status))
      .length;
  }

  filter(status) {
    this.currentPage = 1;
    this.active = status;
  }

  activateModal() {
    this.eventService.showModal(true);
  }
}
