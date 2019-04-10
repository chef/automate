import { Component, Input, Inject } from '@angular/core';
import { Http, Response } from '@angular/http';

@Component({
  selector: 'reset-password',
  template: require('./reset_password.component.html'),
  styles: [ require('./reset_password.component.scss').toString() ]
})

export class ResetPasswordComponent {
  @Input() username: string;
  @Input() token: string;
  enterprise: string;
  password: string = '';
  passwordConfirmation: string = '';
  success: boolean = false;
  canTryAgain: boolean = true;
  errorMessage: string;

  constructor(
    @Inject('Session') session,
    public http: Http
  ) {
    this.enterprise = session.get('enterprise');
  }

  invalidConfirmationPassword(): boolean {
    return this.passwordConfirmation !== '' && !this.validPassword();
  }

  validPassword(): boolean {
    return this.password !== '' && this.password === this.passwordConfirmation;
  }

  newPasswordValue(): boolean {
    return this.password !== '';
  }

  newConfirmationPasswordValue(): boolean {
    return this.passwordConfirmation !== '' && !this.invalidConfirmationPassword();
  }

  submit(): void {
    let body = {
      token: this.token,
      password: this.password
    };
    this.http.post(`/api/v0/e/${this.enterprise}/internal-users/${this.username}/reset-password`, body)
      .subscribe(data => this.gotoLogin(),
                 error => {
                   let resp = error.json();
                   let errMsg = resp.message ? resp.message :
                     error.status ? `${error.status} - ${error.statusText}` : 'Server error';
                   if (error.status !== 500) {
                     this.canTryAgain = false;
                     this.errorMessage = errMsg +
                     ' - Your password reset link expired.' +
                     ' Contact your Chef Automate admin to request a new reset link.';
                   } else {
                     this.errorMessage = errMsg;
                   };
                 });
  }

  gotoLogin(): void {
    window.location.href = `/e/${this.enterprise}/#/login`;
  }
}
