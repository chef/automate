import {
  Component,
  Input,
  OnInit,
  OnChanges,
  SimpleChanges } from '@angular/core';

@Component({
  selector: 'app-policy-groups-list',
  templateUrl: './policy-groups-list.component.html'
})
export class PolicyGroupsListComponent implements OnInit, OnChanges {
  @Input() policyFiles: [];
  @Input() pageOfItems: [];

  constructor() { }

  ngOnInit(): void { }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.pageOfItems) {
      if (changes.pageOfItems.currentValue !== changes.pageOfItems.previousValue) {
        this.policyFiles = this.pageOfItems;
      }
    } else {
      // update list if items array has changed
      if (changes.policyFiles.currentValue !== changes.policyFiles.previousValue) {
        this.policyFiles = this.policyFiles;
      }
    }
  }
}
