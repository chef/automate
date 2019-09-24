import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Credential } from '../credentials.state';
import * as moment from 'moment';
import { CredentialsLogic } from '../credentials.logic';
import { DateTime } from '../../../../helpers/datetime/datetime';

@Component({
  selector: 'app-credentials-list-row',
  templateUrl: './credentials-list-row.html',
  styleUrls: ['./credentials-list-row.scss']
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
<<<<<<< HEAD
<<<<<<< HEAD
    return moment(lastModifiedDate).format(DateTime.RFC2822);
=======
    return moment(lastModifiedDate).format('ddd, DD MMM YYYY HH: mm:ss [UTC]');
>>>>>>> Update time/date format of node credentials last modified column
=======
    console.log(DateTime.RFC2822);
    return moment(lastModifiedDate).format(DateTime.RFC2822);
>>>>>>> Exporting RFC2822 from helper file
  }

  public startCredentialDelete(): void {
    this.deleteModalVisible = true;
  }

  public closeDeleteModal(): void {
    this.deleteModalVisible = false;
  }

}
