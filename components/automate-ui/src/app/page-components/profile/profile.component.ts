import {
  Component, ElementRef, OnInit, ViewChild, ViewChildren, OnDestroy
} from '@angular/core';
import { Subscription, Observable, combineLatest, Subject, of } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { filter, takeUntil, map, distinctUntilChanged } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MetadataService } from 'app/services/metadata/metadata.service';
import { EntityStatus } from 'app/entities/entities';
import {
  WelcomeModalComponent
 } from 'app/page-components/welcome-modal/welcome-modal.component';
import { userSelf, getStatus } from 'app/entities/users/userself.selectors';
import {
  GetUserSelf
 } from 'app/entities/users/userself.actions';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';

@Component({
  selector: 'app-profile',
  templateUrl: './profile.component.html',
  styleUrls: [ './profile.component.scss' ]
})
export class ProfileComponent implements OnInit, OnDestroy {
  dropdownVisible = false;
  buildVersion: string;
  displayName: string;
  isLocalUser: boolean;
  isProfileMenuVisible = true;
  public loading$: Observable<boolean>;
  private isDestroyed = new Subject<boolean>();

  versionSub: Subscription;

  public licenseModalVisible = false;

  @ViewChild(WelcomeModalComponent, { static: true })
  private welcomeModalComponent: WelcomeModalComponent;

  @ViewChildren('focusElement') focusElements;

  constructor(
    private chefSessionService: ChefSessionService,
    private metadataService: MetadataService,
    private store: Store<NgrxStateAtom>,
    public userPrefsService: UserPreferencesService
  ) {
    this.isLocalUser = chefSessionService.isLocalUser;
  }

  ngOnInit() {
    this.displayName = this.chefSessionService.fullname;
    if (this.userPrefsService.uiSettings) {
      this.isProfileMenuVisible = this.userPrefsService.uiSettings.isProfileMenu;
    }

    this.versionSub = this.metadataService.getBuildVersion()
      .subscribe((buildVersion) => {
        this.buildVersion = buildVersion;
      });

    if (this.chefSessionService.isLocalUser) {
      this.store.dispatch(new GetUserSelf());

      this.loading$ = this.store.select(getStatus).pipe(
        map((status: EntityStatus) =>  status !== EntityStatus.loadingSuccess));

      combineLatest([
        this.loading$,
        this.store.select(userSelf)
      ]).pipe(
        filter(([loadingUserData, user]) => !loadingUserData && !isNil(user)),
        map(([_, user]) => user.name),
        distinctUntilChanged(),
        takeUntil(this.isDestroyed)).subscribe(displayName => {
          this.displayName = displayName;
        });
    } else {
      this.loading$ = of(false);
    }
  }

  ngOnDestroy() {
    this.versionSub.unsubscribe();
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  logout() {
    this.chefSessionService.logout('/', true);
  }

  showWelcomeModal() {
    this.welcomeModalComponent.showModal();
    this.closeDropdown();
  }

  toggleDropdown() {
    this.dropdownVisible = !this.dropdownVisible;
  }

  closeDropdown() {
    if (this.dropdownVisible) {
      this.dropdownVisible = false;
    }
  }

  openLicenseModal(): void {
    this.licenseModalVisible = true;
  }

  closeLicenseModal(): void {
    this.licenseModalVisible = false;
  }

  handleArrowUp(event) {
    event.preventDefault();
    const index = this.getCurrentIndex(event.currentTarget);
    const nextElement = this.focusElements.toArray()[index - 1];

    if (nextElement) {
      nextElement.nativeElement.focus();
    }
  }

  handleArrowDown(event) {
    event.preventDefault();
    const index = this.getCurrentIndex(event.currentTarget);
    const nextElement = this.focusElements.toArray()[index + 1];

    if (nextElement) {
      nextElement.nativeElement.focus();
    }
  }

  getCurrentIndex(currentTarget: HTMLElement) {
    return this.focusElements.toArray()
      .map((e: ElementRef) => e.nativeElement)
      .indexOf(currentTarget);
  }

  get userName(): string {
    return this.chefSessionService.username;
  }

  get email(): string {
    return this.chefSessionService.username;
  }
}
