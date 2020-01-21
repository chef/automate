import { Component, Input, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { UsernameMapper } from 'app/helpers/auth/username-mapper';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

@Component({
  selector: 'app-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit, OnDestroy {
  @Input() createUserForm: FormGroup;
  @Input() conflictErrorEvent: EventEmitter<boolean>;
  @Input() passwordErrorEvent: EventEmitter<boolean>;
  @Input() closeEvent: EventEmitter<any>;

  @Output() close = new EventEmitter();

  public conflictError = false;
  public passwordError = false;
  public modifyUsername = false; // Whether the edit Username form is open or not.

  private isDestroyed = new Subject<boolean>();

  ngOnInit(): void {
    this.conflictErrorEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe((isConflict: boolean) => {
        this.conflictError = isConflict;
        // Open the ID input on conflict so user can resolve it.
        this.modifyUsername = isConflict;
      });

    this.passwordErrorEvent.pipe(takeUntil(this.isDestroyed))
      .subscribe((badPassword: boolean) => {
        this.passwordError = badPassword;
        this.createUserForm.get('password').reset();
        this.createUserForm.get('confirmPassword').reset();
      });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
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
        UsernameMapper.transform(this.createUserForm.controls.displayName.value.trim()));
    }
  }

  handleClose(): void {
    this.modifyUsername = false;
  }

  hasAttemptedInput(field: string): boolean {
    return (this.createUserForm.get(field).touched &&
      this.createUserForm.get(field).dirty);
  }

  hasAttemptedInputWithError(field: string): boolean {
    return ((this.createUserForm.get(field).hasError('required') &&
      this.createUserForm.get(field).touched) ||
      (this.createUserForm.get(field).hasError('pattern') &&
        this.createUserForm.get(field).dirty));
  }

  private isNavigationKey(event: KeyboardEvent): boolean {
    return event.key === 'Shift' || event.key === 'Tab';
  }
}
