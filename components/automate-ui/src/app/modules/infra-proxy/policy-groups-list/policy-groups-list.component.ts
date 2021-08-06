import { Component, Input, OnInit, OnChanges, SimpleChanges } from '@angular/core';

@Component({
  selector: 'app-policy-groups-list',
  templateUrl: './policy-groups-list.component.html'
})
export class PolicyGroupsListComponent implements OnInit, OnChanges {
  @Input() policyFiles: [];
  @Input() searchFlag: boolean;
  public policyGroups = [];
  public list = [];

  constructor() { }

  ngOnInit(): void {
    this.filterDataGroupWise();
  }

    ngOnChanges(changes: SimpleChanges) {
    // update list if items array has changed
    if (changes.policyFiles.currentValue !== changes.policyFiles.previousValue) {
      this.list = this.policyFiles;
      this.filterDataGroupWise();
    }
  }

  filterDataGroupWise() {
    const key = 'policy_group';
    this.policyGroups = [];
    if (this.list.length > 0) {
      this.list.forEach((x) => {
        // Checking if there is any object in this.policyGroupsList
        // which contains the key value
        if (this.policyGroups.some((val) => val[key] === x[key])) {
          // If yes! then increase the occurrence by 1
          this.policyGroups.forEach((k) => {
            if (k[key] === x[key]) {
              k['occurrence']++;
            }
          });
        } else {
          // If not! Then create a new object initialize
          // it with the present iteration key's value and set the occurrence to 1
          const a = {};
          a[key] = x[key];
          a['occurrence'] = 1;
          this.policyGroups.push(a);
        }
      });
    } else {
      this.policyGroups = [];
    }
  }
}
