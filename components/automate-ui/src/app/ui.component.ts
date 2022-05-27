import { Component, OnInit, ChangeDetectorRef, AfterViewChecked } from '@angular/core';
import { ActivationStart, ActivationEnd, Router, NavigationEnd } from '@angular/router';
import { Store } from '@ngrx/store';
import { filter } from 'rxjs/operators';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Feature } from 'app/services/feature-flags/types';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { GetAllUserPerms } from './entities/userperms/userperms.actions';
import { AppConfigService } from './services/app-config/app-config.service';
import { GetUserPreferences } from './services/user-preferences/user-preferences.actions';
import { ChefSessionService } from './services/chef-session/chef-session.service';
import { UserPreferencesService } from './services/user-preferences/user-preferences.service';
import { isNull } from 'lodash';

@Component({
  selector: 'app-ui',
  templateUrl: './ui.component.html',
  styleUrls: ['./ui.component.scss']
})
export class UIComponent implements OnInit, AfterViewChecked {
  showBanner = false;
  // Feature Flags
  // TODO:eng-ex This static data seems out of place. Should it go in InitialState?
  experimentalFeatures: Array<Feature> = [
    {
      key: 'chefInfraTabsViews',
      name: 'Chef Infra Server Views (remaining)'
    }
  ];

  betaFeatures: Array<Feature> = [];

  legacyFeatures: Array<Feature> = [];
  hideFullPage = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router,
    public layoutFacade: LayoutFacadeService,
    private cdRef: ChangeDetectorRef,
    private appConfigService: AppConfigService,
    private chefSessionService: ChefSessionService,
    private userPrefsService: UserPreferencesService
  ) {
    // ActivationEnd specifically needs to be here in the constructor to catch early events.
    this.router.events.pipe(
      filter(event => event instanceof ActivationEnd)
    ).subscribe((event: ActivationEnd) => {
      this.hideFullPage = typeof event.snapshot.data.hideNavBar !== 'undefined'
        ? !event.snapshot.data.hideNavBar
        : this.hideFullPage;

      if (this.hideFullPage) {
        this.layoutFacade.hideFullPage();
      } else {
        this.layoutFacade.showFullPage();
      }
    });
  }

  ngAfterViewChecked() {
    this.cdRef.detectChanges();
  }

  ngOnInit(): void {
    this.showBanner = this.appConfigService.showBanner;

    this.router.events.pipe(
        filter(event => event instanceof ActivationStart)
    ).subscribe((event: ActivationStart) => this.hideFullPage = !event.snapshot.data.hideNavBar);

    this.router.events.pipe(
        filter(event => event instanceof NavigationEnd)
    ).subscribe(() => this.store.dispatch(new GetAllUserPerms()));

    // Initial call
    this.store.dispatch(new GetAllUserPerms());
    if (this.chefSessionService.connector && this.userPrefsService.uiSettings.isTimeformatExist) {
      this.store.dispatch(new GetUserPreferences());
    }

    // manual upgrade banner
    const bannerStorage = localStorage.getItem('manual-upgrade-banner');
    if (isNull(bannerStorage)) {
      localStorage.setItem('manual-upgrade-banner', this.booleanToString(true));
      this.showBanner = true;
    } else {
      this.showBanner = this.stringToBoolean(bannerStorage);
    }
    this.showBanner = this.stringToBoolean('false'); // uncomment this line to hide static Banner
  }

  closeBanner() {
    localStorage.setItem('manual-upgrade-banner', this.booleanToString(false));
    this.showBanner = false;
  }

  private booleanToString(bool: boolean): string {
    return bool ? 'true' : 'false';
  }

  private stringToBoolean(boolString: string): boolean {
    return boolString === 'true';
  }
}
