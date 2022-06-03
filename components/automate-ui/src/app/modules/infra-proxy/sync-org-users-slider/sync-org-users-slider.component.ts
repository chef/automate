import { Component, EventEmitter, Input, Output, OnInit, HostBinding, ViewChild, ElementRef } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-sync-org-users-slider',
  templateUrl: './sync-org-users-slider.component.html',
  styleUrls: ['./sync-org-users-slider.component.scss']
})
export class SyncOrgUsersSliderComponent implements OnInit {
  @Input() isUploaded = false;
  @Input() uploadForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() uploadClicked = new EventEmitter();
  @HostBinding('class.active') isSlideOpen = false;

  public uploading = false;
  public conflictError = false;
  public migrationSliderVisible = false;
  public migrationSlider = false;

  @ViewChild('fileDropRef', { static: false }) fileDropEl: ElementRef;
  public file: File = null;
  public isFile = false;

  constructor(private telemetryService: TelemetryService) { }

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
    });
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!Utilities.isNavigationKey(event)) {
      this.conflictError = false;
      this.uploadForm.controls.id.setValue(
        IdMapper.transform(this.uploadForm.controls.file.value.trim()));
    }
  }

  uploadFile(): void {
    this.uploading = true;
    const formData = new FormData();
    this.file = this.uploadForm.get('file').value;
    formData.append('file', this.file);
    this.uploadClicked.emit(this.file);
    this.migrationSlider = true;
    this.telemetryService.track('InfraServer_ChefInfraServer_SyncOrgAndUser_UploadFile');
    this.closeUploadSlider();
  }

  closeUploadSlider() {
    this.uploading = false;
    this.isUploaded = false;
    this.toggleSlide();
    this.file = null;
    this.isFile = false;
    this.isSlideOpen = false;
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel() {
    this.isSlideOpen = true;
  }

  /*** on file drop handler */
  onFileDropped($event: any[]) {
    this.prepareFilesList($event);
  }

  /*** handle file from browsing */
  fileBrowseHandler(file: any) {
    this.prepareFilesList(file);
  }

  prepareFilesList(file: any) {
    this.file = file[0];
    this.uploadForm.get('file').setValue(this.file);
    if ( this.file.type !== 'application/zip' ) {
      this.isFile = false;
      this.file = null;
    } else {
      this.isFile = true;
    }
    this.fileDropEl.nativeElement.value = '';
  }
}
