import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { GetIamVersion } from 'app/entities/policies/policy.actions';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';

@Component({
  selector: 'app-settings-sidebar',
  templateUrl: './settings-sidebar.component.html',
  styleUrls: ['./settings-sidebar.component.scss']
})

export class SettingsSidebarComponent implements OnInit {
  public iamMajorVersion$: Observable<IAMMajorVersion>;
  public iamMinorVersion$: Observable<IAMMinorVersion>;

  constructor(
    private store: Store<NgrxStateAtom>
    ) {
    this.iamMajorVersion$ = store.select(iamMajorVersion);
    this.iamMinorVersion$ = store.select(iamMinorVersion);
  }

  ngOnInit() {
    this.store.dispatch(new GetIamVersion());
  }
}
