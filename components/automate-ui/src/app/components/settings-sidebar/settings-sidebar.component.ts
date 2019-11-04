import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  isIAMv2
} from 'app/entities/policies/policy.selectors';
import { GetIamVersion } from 'app/entities/policies/policy.actions';
import { FeatureFlagsService } from '../../services/feature-flags/feature-flags.service';

@Component({
  selector: 'app-settings-sidebar',
  templateUrl: './settings-sidebar.component.html',
  styleUrls: ['./settings-sidebar.component.scss']
})

export class SettingsSidebarComponent implements OnInit {
  public isIAMv2$: Observable<boolean>;
  featureFlagOn: boolean;

  constructor(
    private store: Store<NgrxStateAtom>,
    private featureFlags: FeatureFlagsService
    ) {
    this.isIAMv2$ = store.select(isIAMv2);
    this.featureFlagOn = this.featureFlags.getFeatureStatus('servicenow_cmdb');
  }

  ngOnInit() {
    this.store.dispatch(new GetIamVersion());
  }
}
