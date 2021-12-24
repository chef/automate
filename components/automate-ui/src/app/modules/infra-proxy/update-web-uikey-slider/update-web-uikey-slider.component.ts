import { Component, HostBinding, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { Utilities } from 'app/helpers/utilities/utilities';

@Component({
  selector: 'app-update-web-uikey-slider',
  templateUrl: './update-web-uikey-slider.component.html',
  styleUrls: ['./update-web-uikey-slider.component.scss']
})
export class UpdateWebUIKeySliderComponent implements OnInit {
  @HostBinding('class.active') isSlideOpen = false;
  @Output() updateWebuiKeyClicked = new EventEmitter();
  @Input() updateWebuiKeyForm: FormGroup;
  @Input() updatingWebuiKey = false;
  @Input() conflictErrorEvent: EventEmitter<boolean>;

  public conflictError = false;

  ngOnInit() {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
    });
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
  }

  updateWebUIkey(): void {
    this.toggleSlide();
    this.updateWebuiKeyClicked.emit();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel() {
    this.isSlideOpen = true;
  }

  closeUpdateModal() {
    this.toggleSlide();
  }
}
