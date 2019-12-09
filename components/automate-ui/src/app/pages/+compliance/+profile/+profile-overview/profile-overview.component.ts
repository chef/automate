import { combineLatest as observableCombineLatest,
         throwError as observableThrowError,
         forkJoin as observableForkJoin,
         Subject,
         Observable } from 'rxjs';

import { map, takeUntil, catchError } from 'rxjs/operators';
import {
  Component,
  OnInit,
  OnDestroy,
  ElementRef,
  ViewChild
} from '@angular/core';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { ProfilesService } from 'app/services/profiles/profiles.service';
import { UploadService } from 'app/services/profiles/upload.service';
import { AvailableProfilesService } from 'app/services/profiles/available-profiles.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { find } from 'lodash';

interface Profile {
    name: String;
    version: String;
    latest_version?: String;
    installed_version?: String;
}

@Component({
  selector: 'app-profiles-overview',
  templateUrl: './profile-overview.component.html',
  styleUrls: ['./profile-overview.component.scss'],
  providers: [AvailableProfilesService, ProfilesService, UploadService]
})

export class ProfileOverviewComponent implements OnInit, OnDestroy {
  // installed and available profiles
  installedProfiles: Array<Profile> = [];
  availableProfiles: Array<Profile> = [];
  user;
  profileUpdatesAvailable: Array<Profile> = [];
  viewAvailableUpdatesList: boolean;

  profilesListLoading = true;
  availableListLoading = true;

  // Search Bar
  filteredProfiles: Array<Profile> = [];
  filteredAvailableProfiles: Array<Profile> = [];
  filteredProfilesLength = 0;
  filteredAvailableProfilesLength = 0;

  // shows setup page when false
  profilesEnabled = true;

  // used for upload component
  uploadModalVisible = false;
  fileUploads = [];
  openUploadResults = {};

  // notification data
  downloadErrorVisible = false;

  userProfilesDataLoaded = false;
  availableProfilesDataLoaded = false;

  // Tabs
  selectedTab: 'installed' | 'available' = 'installed';

  @ViewChild('uploadModal', { static: false }) uploadModal: ElementRef;

  // Empty Page
  profilesEmpty = false;

  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  constructor(
    private profilesService: ProfilesService,
    private availableProfilesService: AvailableProfilesService,
    private uploadService: UploadService,
    private chefSessionService: ChefSessionService,
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar('compliance');
    this.layoutFacade.ShowPageLoading(true);
    this.user = this.chefSessionService.username;

    observableForkJoin([
        this.loadProfiles(),
        this.loadAvailableProfiles()
      ]).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(([profiles, _availableProfiles]) => {
        this.checkForUpdatesAvailable(profiles);
        this.stopSpinnerAfterDataLoad();
      });
  }

  onSearchInput(event) {
    const value = event.target.value;
    const filter = profile => {
      return ['name', 'version', 'title'].some(key => {
        return profile[key].toLowerCase().includes(value.toLowerCase());
      });
    };
    this.filteredProfiles = this.installedProfiles.filter(filter);
    this.filteredAvailableProfiles = this.availableProfiles.filter(filter);
  }

  // load the user's profiles
  loadProfiles() {
    this.profilesEmpty = false;
    this.profilesListLoading = true;
    return this.profilesService.getAllProfiles(this.user).pipe(
      map(profiles => {
        this.profilesListLoading = false;
        this.installedProfiles = profiles;
        this.filteredProfiles = profiles;
        this.profilesEmpty = this.installedProfiles.length === 0 ? true : false;
        this.userProfilesDataLoaded = true;
        return profiles;
      }),
      catchError(resp => {
        if (resp.status === 404) {
          this.checkIfEnabled();
        }
        return observableThrowError(resp);
      }));
  }

  refreshProfiles() {
    this.loadProfiles().subscribe(profiles => {
      this.checkForUpdatesAvailable(profiles);
    });
  }

  // load profiles available to the user
  loadAvailableProfiles() {
    this.availableListLoading = true;
    return this.availableProfilesService.getAllProfiles().pipe(
      map(availableProfiles => {
        this.availableListLoading = false;
        this.availableProfiles = availableProfiles;
        this.filteredAvailableProfiles = availableProfiles;
        this.availableProfilesDataLoaded = true;
        return availableProfiles;
      }),
      catchError(resp => {
        if (resp.status === 404) {
          this.checkIfEnabled();
        }
        return observableThrowError(resp);
      }));
  }

  stopSpinnerAfterDataLoad() {
    if (this.userProfilesDataLoaded && this.availableProfilesDataLoaded) {
      this.layoutFacade.ShowPageLoading(false);
    }
  }

  checkForUpdatesAvailable(profiles: Array<Profile>) {
    this.profileUpdatesAvailable = [];
    // filter list of profiles to return name, latest_version
    // of each profile if latest_version field exists
    const profilesToUpdate = profiles.filter((profile: Profile) => {
      if (profile.latest_version) {
        return { name: profile.name, version: profile.version, latest: profile.latest_version };
      }
    });

    // find profile from available profiles and push to array
    profilesToUpdate.forEach((profile) => {
      const newerVersionProfile = find(this.availableProfiles, function(availProfile: Profile) {
        return availProfile.name === profile.name;
      });

      // look for the matching profile, because we only add the profile
      // to the updates available array if it's not already installed
      const alreadyInstalled = find(this.installedProfiles, function(installedProfile: Profile) {
        return (installedProfile.name === newerVersionProfile.name &&
          installedProfile.version === newerVersionProfile.version);
      });

      // include installed version in available profile for reference
      newerVersionProfile.installed_version = profile.version;

      if (alreadyInstalled === undefined) {
        this.profileUpdatesAvailable.push(newerVersionProfile);
      }
    });
  }

  showAvailableUpdates() {
    this.viewAvailableUpdatesList = true;
  }

  hideAvailableUpdates() {
    this.viewAvailableUpdatesList = false;
  }

  checkIfEnabled() {
    this.profilesService.getVersion().pipe(
      takeUntil(this.isDestroyed))
      .subscribe(
        () => {
          this.profilesEnabled = true;
        },
        (resp) => {
          this.profilesEnabled = resp.status === 404 ? false : true;
        });
  }

  onFilesSelected(event) {
    this.sendFiles(Array.from(event.target.files)).subscribe({
      next: fileUploads => { this.fileUploads = fileUploads; },
      complete: () => {
        this.refreshProfiles();
        const failures = this.fileUploads.some(f => f.failed);
        if (!failures) {
          this.hideUploadModal();
          this.fileUploads = [];
        }
      }
    });
  }

  sendFiles(files) {
    return observableCombineLatest([
      ...files.map(file => this.uploadService.sendFile(file))
    ]);
  }

  trackBy(file) {
    return file.name;
  }

  // used for uploading tar.gz files retrieved from api
  getProfiles(profiles) {
    // nav to profiles tab
    this.selectedTab = 'installed';

    const installProfiles: Array<Observable<any>> = profiles.map(profile => {
      return this.availableProfilesService.installMarketProfile(profile.name, profile.version);
    });
    observableForkJoin(installProfiles).pipe(
      takeUntil(this.isDestroyed))
      .subscribe(
        () => this.refreshProfiles(),
        () => this.showDownloadError()
      );
  }

  showDownloadError() {
    this.downloadErrorVisible = true;
  }

  hideDownloadError() {
    this.downloadErrorVisible = false;
  }

  showUploadModal() {
    this.uploadModalVisible = true;
  }

  hideUploadModal() {
    this.uploadModalVisible = false;
  }

  isOpenUploadResult(file) {
    return this.openUploadResults[file.name];
  }

  toggleUploadResult(file) {
    this.openUploadResults[file.name] = !this.openUploadResults[file.name];
  }

  onTabChange(event) {
    this.selectedTab = event.target.value;
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }
}
