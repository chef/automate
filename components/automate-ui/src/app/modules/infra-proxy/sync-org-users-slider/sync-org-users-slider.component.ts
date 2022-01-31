import { Component, EventEmitter, Input, Output, OnInit, HostBinding, ViewChild, ElementRef } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';

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
  public file: any;
  public isFile = false;

  constructor() { }

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
    formData.append('file', this.uploadForm.get('file').value);
    this.uploadClicked.emit(this.uploadForm.get('file').value);
    this.migrationSlider = true;
  }

  closeUploadSlider() {
    this.uploading = false;
    this.isUploaded = false;
    this.toggleSlide();
    this.file = '';
    this.isFile = false;
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel() {
    this.isSlideOpen = true;
  }

  public openMigrationSlider(): void {
    this.migrationSliderVisible = true;
    this.resetMigrationSlider();
  }

  public closeMigrationSlider(): void {
    this.migrationSliderVisible = false;
    this.resetMigrationSlider();
  }

  private resetMigrationSlider(): void {
    this.conflictErrorEvent.emit(false);
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
      this.file = '';
    } else {
      this.isFile = true;
    }
    this.fileDropEl.nativeElement.value = '';
  }
}
