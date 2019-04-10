import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy } from '@angular/core';
import { Credential, CredentialsList } from '../credentials.state';

@Component({
  selector: 'app-credentials-list',
  changeDetection: ChangeDetectionStrategy.OnPush,
  templateUrl: './credentials-list.html',
  styleUrls: ['./credentials-list.scss']
})
export class CredentialsListComponent {
  @Input()
  credentialsList: CredentialsList | null;
  @Output()
  deleteCredential: EventEmitter<Credential> = new EventEmitter<Credential>();
  @Output()
  pageChanged: EventEmitter<number> = new EventEmitter<number>();
  @Output()
  sortToggled: EventEmitter<any> = new EventEmitter<any>();

  onPageChanged(page) {
    this.pageChanged.emit(page);
  }

  onSortToggled(event) {
    this.sortToggled.emit(event.detail);
  }

  orderFor(sortKey) {
    const {sort, order} = this.credentialsList;
    if (sortKey === sort) {
      return order;
    }
    return 'none';
  }
}
