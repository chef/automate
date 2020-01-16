import { Component, Input, OnInit, EventEmitter } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { IdMapper } from 'app/helpers/auth/id-mapper';

@Component({
  selector: 'app-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit {
  @Input() createUserForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Input() passwordErrorEvent: EventEmitter<boolean>;

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
      console.log('in the handleName here is username' + this.createUserForm.controls.username.value);
      this.conflictError = false;
      this.createUserForm.controls.username.setValue(
        IdMapper.transform(this.createUserForm.controls.displayname.value.trim()));
        console.log('in the transformation here is username' + this.createUserForm.controls.username.value);
    }
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
