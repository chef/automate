import { Component, Input, Output, OnInit, EventEmitter } from '@angular/core';
import { MatOptionSelectionChange } from '@angular/material/core/option';
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
  @Input() showEmptyMessage: boolean;
  @Input() showTable: boolean;

  @Output() addClicked = new EventEmitter();
  @Output() removeClicked = new EventEmitter<User>();

  // These will default to baseUrl + {get, post}, but can be overridden
  // to make this work with parameterized endpoints. For an example, see
  // team-details.component.html.
  @Input() getPermissionsPath: string[];
  @Input() createPermissionsPath: string[];

  ngOnInit(): void {
    if (this.getPermissionsPath === undefined) {
      this.getPermissionsPath = [this.baseUrl, 'get'];
    }
    if (this.createPermissionsPath === undefined) {
      this.createPermissionsPath = [this.baseUrl, 'post'];
    }
  }

  deleteUser($event: MatOptionSelectionChange, user: User) {
    if ($event.isUserInput) {
      this.removeClicked.emit(user);
    }
  }
}
