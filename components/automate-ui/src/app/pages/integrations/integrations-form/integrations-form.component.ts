import { Component, Input } from '@angular/core';
import { FormGroup, FormArray, FormBuilder } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { get } from 'lodash/fp';

import { NgrxStateAtom } from '../../../ngrx.reducers';
import { Credential } from '../../../entities/credentials/credential.model';
import { instanceCredentials } from '../../../entities/credentials/credential.selectors';
import { SearchCredentials } from '../../../entities/credentials/credential.actions';

@Component({
  selector: 'app-integrations-form',
  templateUrl: './integrations-form.component.html',
  styleUrls: ['./integrations-form.component.scss']
})
export class IntegrationsFormComponent {
  instanceCredentials$: Observable<Credential[]>;

  @Input() integrationsForm: FormGroup;

  constructor(store: Store<NgrxStateAtom>,
    private fb: FormBuilder) {
    this.instanceCredentials$ = store.select(instanceCredentials);
    store.dispatch(new SearchCredentials({}));
  }

  get instance_credentials(): FormArray {
    return this.integrationsForm.get('instance_credentials') as FormArray;
  }

  showInstanceCreds() {
    const formData = this.integrationsForm.value;
    const managerType = get('type', formData);
    return managerType !== 'gcp' && !get([managerType, 'no_creds'], formData);
  }

  handleNewCred() {
    this.instance_credentials.push(this.fb.group({
      tag_key: null,
      tag_value: null,
      credential_ids: null
    }));
  }

  handleRemoveCred(i) {
    this.instance_credentials.removeAt(i);
  }
}
