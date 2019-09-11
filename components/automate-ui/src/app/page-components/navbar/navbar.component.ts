import { Component, isDevMode, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { atLeastV2p1 } from 'app/entities/policies/policy.selectors';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})

export class NavbarComponent implements OnInit {
  public applicationsFeatureFlagOn: boolean;
  public projectsEnabled$: Observable<boolean>;

  constructor(
    private featureFlagsService: FeatureFlagsService,
    private store: Store<NgrxStateAtom>
  ) {}

  isDevMode() {
    return isDevMode();
  }

  ngOnInit() {
    this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
    this.projectsEnabled$ = this.store.select(atLeastV2p1);
  }
}
