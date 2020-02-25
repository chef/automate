import { filter } from 'rxjs/operators';
import { Component, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Subscription } from 'rxjs';
import { Store } from '@ngrx/store';
import {
  filter as lodashFilter,
  get,
  split,
  identity,
  isEmpty,
  pipe,
  mapKeys,
  toUpper,
  toPairs,
  map
} from 'lodash/fp';

import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from '../../../ngrx.reducers';
import { managerFromRoute } from '../../../entities/managers/manager.selectors';
import { UpdateManager } from '../../../entities/managers/manager.actions';
import { Status } from './integrations-edit.reducer';
import { integrationsEditState } from './integrations-edit.selectors';

@Component({
  selector: 'app-integrations-edit',
  templateUrl: './integrations-edit.component.html',
  styleUrls: ['./integrations-edit.component.scss']
})
export class IntegrationsEditComponent implements OnDestroy {
  integrationsForm: FormGroup;
  managerID: string;
  credentialId: string;
  status = Status.notUpdated;

  private subs: Subscription[];

  constructor(
    private store: Store<NgrxStateAtom>,
    location: Location,
    fb: FormBuilder,
    private layoutFacade: LayoutFacadeService
  ) {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.createForm(fb);

    this.subs = [
      store.select(managerFromRoute).pipe(
        filter(identity))
        .subscribe((state) => {
          this.managerID = get('id', state);
          this.credentialId = get('credential_id', state);
          this.loadForm(state, fb);
        }),

      store.select(integrationsEditState)
        .subscribe(({status}) => {
          this.status = status;
          if (status === Status.success) {
            location.back();
          }
        })
    ];
  }

  ngOnDestroy() {
    this.subs.forEach((sub) => sub.unsubscribe());
  }

  createForm(fb) {
    this.integrationsForm = fb.group({
      name: ['', Validators.required],
      type: 'aws',
      aws: fb.group({
        service_type: null,
        no_creds: false,
        credentials: fb.group({
          aws_access_key_id: '',
          aws_secret_access_key: ''
        })
      }),
      azure: fb.group({
        service_type: null,
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

  loadForm(state, fb) {
    const name = get('name', state);
    const type = split('-', get('type', state))[0];
    const service_type = get('type', state);
    const no_creds = !this.credentialId;

    if (no_creds) {
      const credGroupFn = ({tag_key, tag_value, credential_ids}) =>
        fb.group({ tag_key, tag_value, credential_ids: [credential_ids] });

      const instance_credentials = fb.array(pipe(get('instance_credentials'),
                                                 map(credGroupFn))(state));

      this.integrationsForm.setControl('instance_credentials', instance_credentials);
    }

    this.integrationsForm.patchValue({
      name,
      type,
      [type]: {
        service_type,
        no_creds
      }
    });
  }

  isUpdating() {
    return this.status === Status.updating;
  }

  handleSave() {
    const formData = this.integrationsForm.value;

    const managerType = get('type', formData);
    const type = get([managerType, 'service_type'], formData);
    const no_creds = get([managerType, 'no_creds'], formData);

    const [credentialData, instanceCredentials] =
      no_creds ? [null, null] : this.getCredentials(formData);

    this.store.dispatch(new UpdateManager({
      id: this.managerID,
      name: get('name', formData),
      type,
      instanceCredentials,
      ...isEmpty(credentialData) ? { credentialId: this.credentialId } : { credentialData }
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
        lodashFilter(([_key, value]: [string, string[]]) => !!value),
        map(([key, value]) => ({ key, value }))
      )(formData),

      // instanceCredentials
      get('instance_credentials', formData)
    ];
  }
}
