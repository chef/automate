import { Component, OnInit, ViewChild, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { MetadataService } from '../../services/metadata/metadata.service';
import { WelcomeModalComponent } from '../welcome-modal/welcome-modal.component';

@Component({
  selector: 'app-profile',
  templateUrl: './profile.component.html',
  styleUrls: [ './profile.component.scss' ]
})
export class ProfileComponent implements OnInit, OnDestroy {
  dropdownVisible = false;

  buildVersion: string;

  versionSub: Subscription;

  public licenseModalVisible = false;

  @ViewChild(WelcomeModalComponent)
  private welcomeModalComponent: WelcomeModalComponent;

  constructor(
    private chefSessionService: ChefSessionService,
    private metadataService: MetadataService
  ) { }

  ngOnInit() {
    this.versionSub = this.metadataService.getBuildVersion()
      .subscribe((buildVersion) => {
        this.buildVersion = buildVersion;
      });
  }

  ngOnDestroy() {
    this.versionSub.unsubscribe();
  }

  logout() {
    this.chefSessionService.logout();
  }

  showWelcomeModal() {
    this.welcomeModalComponent.showModal();
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

  get displayName(): string {
    return this.chefSessionService.fullname;
  }

  get userName(): string {
    return this.chefSessionService.username;
  }

  get email(): string {
    return this.chefSessionService.username;
  }
}
