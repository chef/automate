import { Component, EventEmitter, Input, Output, OnInit, HostBinding } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';
import { Utilities } from 'app/helpers/utilities/utilities';

@Component({
  selector: 'app-sync-org-users-slider',
  templateUrl: './sync-org-users-slider.component.html',
  styleUrls: ['./sync-org-users-slider.component.scss']
})
export class SyncOrgUsersSliderComponent implements OnInit {
  @Input() visible = false;
  @Input() uploading = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Output() close = new EventEmitter();
  @Output() uploadClicked = new EventEmitter();
  @Input() uploadForm: FormGroup;
  @HostBinding('class.active') isSlideOpen = false;

  public conflictError = false;
  fileInfo: string;

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
        IdMapper.transform(this.uploadForm.controls.name.value.trim()));
    }
  }

  uploadFile(): void {
    // this.toggleSlide();
    this.uploadClicked.emit();
  }

  closeUploadSlider() {
    this.toggleSlide();
    this.close.emit();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel() {
    this.isSlideOpen = true;
  }

  onFileSelect(input: HTMLInputElement): void {
    const file = input.files[0];
    this.fileInfo = `${file.name}`;
  }
}
