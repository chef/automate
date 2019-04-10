import { Component, Input, Output, EventEmitter } from '@angular/core';
import { Credential } from '../credentials.state';
import * as moment from 'moment';
import { CredentialsLogic } from '../credentials.logic';

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

  constructor(public credentialsLogic: CredentialsLogic) {
  }

  getLastModified(lastModifiedDate): string {
    return moment(lastModifiedDate).format('MMMM Do YYYY');
  }
}
