import { Component, OnInit } from '@angular/core';
import { ActivationStart, ActivationEnd, Router, NavigationEnd } from '@angular/router';
import { Store } from '@ngrx/store';
import { filter } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Feature } from 'app/services/feature-flags/types';

import { GetIamVersion } from 'app/entities/policies/policy.actions';
import { GetAllUserPerms } from './entities/userperms/userperms.actions';

@Component({
  selector: 'app-ui',
  templateUrl: './ui.component.html',
  styleUrls: ['./ui.component.scss']
})
export class UIComponent implements OnInit {
  // Feature Flags
  // TODO:eng-ex This static data seems out of place. Should it go in InitialState?
  experimentalFeatures: Array<Feature> = [];
  betaFeatures: Array<Feature> = [
    {
      key: 'applications',
      name: 'EAS Applications'
    },
    {
      key: 'servicenow_cmdb',
      name: 'ServiceNow CMDB Integration'
    }
  ];

  legacyFeatures: Array<Feature> = [];

  renderNavbar = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router
  ) {

    // ActivationEnd specifically needs to be here in the constructor to catch early events.
    this.router.events.pipe(
      filter(event => event instanceof ActivationEnd)
    ).subscribe((event: ActivationEnd) => {
      this.renderNavbar = typeof event.snapshot.data.hideNavBar !== 'undefined'
        ? !event.snapshot.data.hideNavBar
        : this.renderNavbar;
    });
  }

  ngOnInit(): void {
    this.router.events.pipe(
        filter(event => event instanceof ActivationStart)
    ).subscribe((event: ActivationStart) => this.renderNavbar = !event.snapshot.data.hideNavBar);

    this.router.events.pipe(
        filter(event => event instanceof NavigationEnd)
    ).subscribe(() => this.store.dispatch(new GetAllUserPerms()));

    // Initial call
    this.store.dispatch(new GetAllUserPerms());

    // Initial calls for polled events
    this.store.dispatch(new GetIamVersion());
  }
}
