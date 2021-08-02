import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'app-policy-groups-list',
  templateUrl: './policy-groups-list.component.html',
  styleUrls: ['./policy-groups-list.component.scss']
})
export class PolicyGroupsListComponent implements OnInit {
  @Input() policyGroups: [];
  @Input() policyGroupsListLoading: boolean;

  public policyGroupsList = [];
  
  constructor() { }

  ngOnInit(): void {
    this.filterDataGroupWise();
  }

  filterDataGroupWise() {
    const key = 'policy_group';
    this.policyGroups.forEach((x) => {
      // Checking if there is any object in this.policyGroupsList
      // which contains the key value
      if (this.policyGroupsList.some((val) => val[key] === x[key])) {
        // If yes! then increase the occurrence by 1
        this.policyGroupsList.forEach((k) => {
          if (k[key] === x[key]) {
            k['occurrence']++;
          console.log(x);
          }
        });
      } else {
        // If not! Then create a new object initialize
        // it with the present iteration key's value and set the occurrence to 1
        const a = {};
        a[key] = x[key];
        a['occurrence'] = 1;
        this.policyGroupsList.push(a);
        console.log(a);
      }
    });
  }

}
