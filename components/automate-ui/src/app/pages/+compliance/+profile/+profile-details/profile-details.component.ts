import { takeUntil } from 'rxjs/operators';
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subject } from 'rxjs';
import { ProfilesService } from 'app/services/profiles/profiles.service';
import { AvailableProfilesService } from 'app/services/profiles/available-profiles.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

import { saveAs } from 'file-saver';

@Component({
  selector: 'app-profile-details',
  templateUrl: './profile-details.component.html',
  styleUrls: ['./profile-details.component.scss'],
  providers: [AvailableProfilesService, ProfilesService]
})
export class ProfileDetailsComponent implements OnInit, OnDestroy {
  profile: any = {};
  isAvailable = false;
  isInstalled = false;
  showLoadingIcon = false;

  // notification data
  deleteErrorVisible = false;
  downloadErrorVisible = false;
  installErrorVisible = false;

  openControls: any = {};

  deleteModalVisible = false;

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private profilesService: ProfilesService,
    private availableProfilesService: AvailableProfilesService,
    private chefSessionService: ChefSessionService,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar('compliance');
    this.layoutFacade.ShowPageLoading(false);
    this.route.queryParams.pipe(
      takeUntil(this.isDestroyed))
      .subscribe(({owner, name, version}) => {
        this.isInstalled = owner && owner.length > 0;
        this.fetchProfile(owner, name, version);
      });
  }

  showDeleteModal() {
    this.deleteModalVisible = true;
  }

  hideDeleteModal() {
    this.deleteModalVisible = false;
  }

  deleteProfile(profile) {
    this.profilesService
      .deleteProfile(this.chefSessionService.username, profile.name, profile.version)
      .subscribe(
        () => this.routeToOverview(),
        () => this.showDeleteError()
     );
  }

  installProfile(profile) {
    this.availableProfilesService.installMarketProfile(profile.name, profile.version)
       .subscribe(data => {
          if (data) {
            this.isAvailable = false;
            // once the file has been uploaded, find username (i.e. owner) and fetchProfile,
            // and route to the "installed" profile so url is updated with correct information
            const username = this.chefSessionService.username;
            this.fetchProfile(username, profile.name, profile.version);
            this.routeToProfile(username, profile.name, profile.version);
          }
        },
        () => this.showInstallError()
      );
  }

  showDownloadError() {
    this.downloadErrorVisible = true;
  }

  hideDownloadError() {
    this.downloadErrorVisible = false;
  }

  showDeleteError() {
    this.deleteErrorVisible = true;
  }

  hideDeleteError() {
    this.deleteErrorVisible = false;
  }

  showInstallError() {
    this.installErrorVisible = true;
  }

  hideInstallError() {
    this.installErrorVisible = false;
  }

  fetchProfile(owner, name, version) {
    this.showLoadingIcon = true;
    this.layoutFacade.ShowPageLoading(true);
    if (!owner || !owner.length) {
    // if we receive undefined for owner, we know this
    // profile is a 'premium' (available) profile
      this.availableProfilesService.getProfile(name, version).pipe(
        takeUntil(this.isDestroyed))
        .subscribe(
          (availProfile) => {
            this.profile = availProfile;
            this.isAvailable = true;
            this.showLoadingIcon = false;
            this.layoutFacade.ShowPageLoading(false);
          });
    } else {
      this.profilesService.getProfile(owner, name, version).pipe(
        takeUntil(this.isDestroyed))
        .subscribe(
          (installedProfile) => {
            this.profile = installedProfile;
            this.showLoadingIcon = false;
            this.layoutFacade.ShowPageLoading(false);
          });
    }
  }

  downloadProfile(profile) {
    const {owner, name, version} = profile;

    if (!owner || !owner.length) {
      this.availableProfilesService.getTarFile(name, version)
        .subscribe(tarball => {
          saveAs(tarball, `${name}-${version}.tar.gz`);
        },
        () => this.showDownloadError()
      );
    } else {
      this.profilesService.getTarFile(owner, name, version)
        .subscribe(tarball => {
          saveAs(tarball, `${owner}-${name}-${version}.tar.gz`);
        },
        () => this.showDownloadError()
      );
    }
  }

  routeToOverview() {
    this.router.navigate(['/profiles']);
  }

  routeToProfile(owner, name, version) {
    this.router.navigate(
      ['/profiles', 'profile-details'],
      {queryParams: {owner, name, version}}
    );
  }

  isOpenControl({ id }) {
    return this.openControls[id];
  }

  toggleControl({ id }) {
    this.openControls[id] = !this.openControls[id];
  }

  impactStatus({ impact }) {
    if (impact >= 0.7) { return 'critical'; }
    if (impact >= 0.4) { return 'major'; }
    return 'minor';
  }

  testCount({ code }) {
    return (code.match(/describe/g) || []).length;
  }

  displaySupports(arraySupports) {
    if (arraySupports === undefined) {
      return '';
    }
    return arraySupports.map(s => {
      return [s['os_family'], s['os_name'], s['release'], s['platform']]
        .filter(v => typeof v === 'string')
        .join(' ');
    }).join('; ');
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
