import { throwError as observableThrowError, of as observableOf,  Observable } from 'rxjs';

import { catchError } from 'rxjs/operators';
import { Component, ChangeDetectorRef, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Credential, CredentialTypes } from '../credentials.state';
import { ActivatedRoute, Router } from '@angular/router';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { CredentialsLogic } from '../credentials.logic';
import { environment } from '../../../../../environments/environment';
const SECRETS_URL = environment.secrets_url;

@Component({
  selector: 'app-credentials-add-edit-screen',
  templateUrl: './credentials-add-edit-screen.html'
})

export class CredentialsAddEditScreenComponent implements OnInit {
  selectedTab: CredentialTypes = CredentialTypes.SSH;
  saveErrorOccurred = false;
  saveErrorMessage = '';
  lastSubmittedFormType: CredentialTypes = CredentialTypes.SSH;

  credential$: Observable<Credential>;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private httpClient: HttpClient,
    private ref: ChangeDetectorRef,
    private credentialsLogic: CredentialsLogic,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar('settings');
    this.route.paramMap
      .subscribe(params => {
        this.credential$ = params.get('id') ?
          this.fetchCred(params.get('id')) :
          this.blankCred();
      });
  }

  fetchCred(id: string): Observable<Credential> {
    return this.httpClient.get<Credential>(`${SECRETS_URL}/id/${id}`);
  }

  blankCred(): Observable<Credential> {
    return observableOf({name: '', type: CredentialTypes.SSH, data: [], tags: []});
  }

  handleSaveCredential(credential: Credential) {
    this.lastSubmittedFormType =
      this.credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);

    this.saveErrorOccurred = false;

    const saveReq = credential.id ?
      this.httpClient.patch<Credential>(`${SECRETS_URL}/id/${credential.id}`, credential) :
      this.httpClient.post<Credential>(`${SECRETS_URL}`, credential);

    saveReq.pipe(
      catchError(error => {
        this.saveErrorOccurred = true;
        this.saveErrorMessage = error._body || '';
        this.ref.markForCheck();

        return observableThrowError(this.saveErrorMessage);
      }))
      .subscribe(() => {
        return this.router.navigate(['/settings', 'node-credentials']);
      });
  }
}
