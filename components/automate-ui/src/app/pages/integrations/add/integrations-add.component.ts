import { Component, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Subscription } from 'rxjs';
import {
  filter,
  get,
  map,
  mapKeys,
  pipe,
  toPairs,
  toUpper
} from 'lodash/fp';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { CreateManager } from '../../../entities/managers/manager.actions';
import { integrationsAddState } from './integrations-add.selectors';
import { Status } from './integration-add.reducer';

@Component({
  selector: 'app-integrations-add',
  templateUrl: './integrations-add.component.html',
  styleUrls: ['./integrations-add.component.scss']
})
export class IntegrationsAddComponent implements OnDestroy {
  integrationsForm: FormGroup;
  status = Status.notCreated;

  private sub: Subscription;

  constructor(
    private store: Store<NgrxStateAtom>,
    private layoutFacade: LayoutFacadeService,
    location: Location,
    fb: FormBuilder
  ) {
    this.layoutFacade.showSidebar('settings');
    this.sub = store.select(integrationsAddState).subscribe(({status}) => {
      this.status = status;
      if (status === Status.success) {
        location.back();
      }
    });

    this.integrationsForm = fb.group({
      name: ['', Validators.required],
      type: 'aws',
      aws: fb.group({
        service_type: 'aws-api',
        no_creds: false,
        credentials: fb.group({
          aws_access_key_id: '',
          aws_secret_access_key: '',
          aws_region: ''
        })
      }),
      azure: fb.group({
        service_type: 'azure-vm',
        no_creds: false,
        credentials: fb.group({
          azure_client_id: '',
          azure_client_secret: '',
          azure_tenant_id: ''
        })
      }),
      gcp: fb.group({
        service_type: 'gcp-api',
        no_creds: false,
        credentials: fb.group({
          google_credentials_json: ''
        })
      }),
      instance_credentials: fb.array([])
    });
  }

  ngOnDestroy() {
    this.sub.unsubscribe();
  }

  isSaving() {
    return this.status === Status.saving;
  }

  handleSave() {
    const formData = this.integrationsForm.value;
    const managerType = get('type', formData);
    const type = get([managerType, 'service_type'], formData);
    const no_creds = get([managerType, 'no_creds'], formData);

    const [credentialData, instanceCredentials] =
      no_creds ? [null, null] : this.getCredentials(formData);

    this.store.dispatch(new CreateManager({
      name: get('name', formData),
      type,
      credentialData,
      instanceCredentials
    }));

  }

  getCredentials(formData) {
    const managerType = get('type', formData);
    return [
      // credentialData
      pipe(
        get([managerType, 'credentials']),
        mapKeys(toUpper),
        toPairs,
        filter(([_key, value]: [string, string[]]) => !!value),
        map(([key, value]) => ({ key, value }))
      )(formData),

      // instanceCredentials
      get('instance_credentials', formData)
    ];
  }
}
