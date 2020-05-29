import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Credential } from '../credentials.state';
import * as moment from 'moment/moment';
import { DateTime } from '../../../../helpers/datetime/datetime';
import { CredentialsLogic } from '../credentials.logic';

@Component({
  selector: 'app-credentials-list-row',
  templateUrl: './credentials-list-row.html'
})
export class CredentialsListRowComponent {
  @Input()
  // wacky typescript bug workaround, see:
  // https://github.com/angular/angular-cli/issues/2034#issuecomment-304406270
  credential: Credential | Credential;
  @Output()
  deleteCredential: EventEmitter<Credential> = new EventEmitter<Credential>();
  public deleteModalVisible = false;

  constructor(public credentialsLogic: CredentialsLogic) {
  }

  getLastModified(lastModifiedDate): string {
    return moment.utc(lastModifiedDate).format(DateTime.RFC2822);
  }

  public startCredentialDelete(): void {
    this.deleteModalVisible = true;
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

}
