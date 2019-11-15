import { Component, Input, Output, OnInit, EventEmitter } from '@angular/core';
import { User } from 'app/entities/users/user.model';

@Component({
  selector: 'app-user-table',
  templateUrl: './user-table.component.html',
  styleUrls: ['./user-table.component.scss']
})
export class UserTableComponent implements OnInit {
  @Input() addButtonText: string;
  @Input() removeText: string;
  @Input() addButtonEnabled = true;
  @Input() users: User[];
  @Input() baseUrl: string;
  // Needed for team-create which doesn't need to check permissions
  // because table is being populated with data that's already been checked.
  @Input() overridePermissionsCheck = false;
  @Input() showEmptyMessage: boolean;
  @Input() showTable: boolean;

  @Output() addClicked = new EventEmitter();
  @Output() removeClicked = new EventEmitter<User>();

  getPermissionsPath: string[];
  createPermissionsPath: string[];

  constructor() {}

  ngOnInit(): void {
    this.getPermissionsPath = [this.baseUrl, 'get'];
    this.createPermissionsPath = [this.baseUrl, 'post'];
  }
}
