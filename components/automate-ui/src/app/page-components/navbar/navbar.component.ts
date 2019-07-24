import { Component, isDevMode, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { iamMinorVersion, iamMajorVersion } from 'app/entities/policies/policy.selectors';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})

export class NavbarComponent implements OnInit {
  public applicationsFeatureFlagOn: boolean;

  public iamMajorVersion$: Observable<IAMMajorVersion>;
  public iamMinorVersion$: Observable<IAMMinorVersion>;

  constructor(
    private featureFlagsService: FeatureFlagsService,
    private store: Store<NgrxStateAtom>
  ) {}

  isDevMode() {
    return isDevMode();
  }

  ngOnInit() {
    this.applicationsFeatureFlagOn = this.featureFlagsService.getFeatureStatus('applications');
    this.iamMajorVersion$ = this.store.select(iamMajorVersion);
    this.iamMinorVersion$ = this.store.select(iamMinorVersion);
  }
}
