import {
  Component,
  Input,
  OnChanges,
  SimpleChanges } from '@angular/core';

@Component({
  selector: 'app-policy-groups-list',
  templateUrl: './policy-groups-list.component.html'
})
export class PolicyGroupsListComponent implements OnChanges {
  @Input() policyFiles: [];
  @Input() pageOfItems: [];

  constructor() { }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.pageOfItems) {
      if (changes.pageOfItems.currentValue !== changes.pageOfItems.previousValue) {
        this.policyFiles = this.pageOfItems;
      }
    }
  }
}
