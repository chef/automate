import { Component, Input, OnInit, EventEmitter, Output } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { UsernameMapper } from 'app/helpers/auth/username-mapper';

@Component({
  selector: 'app-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit {
  @Input() createUserForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Input() passwordErrorEvent: EventEmitter<boolean>;
  @Input() closeEvent: EventEmitter<any>;

  @Output() close = new EventEmitter();

  public conflictError = false;
  public passwordError = false;
  public modifyUsername = false; // Whether the edit Username form is open or not.

  ngOnInit(): void {
    this.conflictErrorEvent.subscribe((isConflict: boolean) => {
      this.conflictError = isConflict;
      // Open the ID input on conflict so user can resolve it.
      this.modifyUsername = isConflict;
    });
    // TODO add takeUntil(this.isDestroyed) to unsubscribe

    this.passwordErrorEvent.subscribe((badPassword: boolean) => {
      this.passwordError = badPassword;
      this.createUserForm.get('password').reset();
      this.createUserForm.get('confirmPassword').reset();
    });
    // TODO add takeUntil(this.isDestroyed) to unsubscribe
  }

  handleUsernameInput(event: KeyboardEvent): void {
    if (this.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  handleNameInput(event: KeyboardEvent): void {
    if (!this.modifyUsername && !this.isNavigationKey(event)) {
      this.conflictError = false;
      this.createUserForm.controls.username.setValue(
        UsernameMapper.transform(this.createUserForm.controls.displayname.value.trim()));
    }
  }

  handleClose(): void {
    this.modifyUsername = false;
    this.close.emit();
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
