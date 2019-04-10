import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { iamMajorVersion, iamMinorVersion } from 'app/entities/policies/policy.selectors';
import { GetIamVersion } from 'app/entities/policies/policy.actions';

@Component({
  selector: 'app-admin-sidebar',
  templateUrl: './admin-sidebar.component.html',
  styleUrls: ['./admin-sidebar.component.scss']
})

export class AdminSidebarComponent implements OnInit {
  public iamMajorVersion$: Observable<string>;
  public iamMinorVersion$: Observable<string>;

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
