import { Injectable } from '@angular/core';
import { Credential,
         TouchCapableDirtyCapableFormControl,
         CredentialTypes } from './credentials.state';

@Injectable()
export class CredentialsLogic {
  getCssClassFromCredentialType(credentialType: CredentialTypes): string {
    switch (credentialType) {
      case CredentialTypes.SSH:
        return 'fa:fa-terminal';
      case CredentialTypes.WinRM:
        return 'fa:fa-windows';
      case CredentialTypes.Sudo:
        return 'fa:fa-level-up';
      default:
        return 'fa:fa-lock';
    }
  }

  formatKeyType(credentialType: CredentialTypes): string {
    switch (credentialType) {
      case CredentialTypes.SSH:
        return 'SSH';
      case CredentialTypes.WinRM:
        return 'WinRM';
      case CredentialTypes.Sudo:
        return 'Sudo';
      default:
        return 'SSH';
    }
  }

  showFormControlValidationError(formControl: TouchCapableDirtyCapableFormControl): boolean {
    return formControl.invalid &&
      formControl.touched;
  }

  // return true if they are both invalid and either are touched...
  // return false for every other combination
  showSSHPasswordOrSSHPublicKeyFormControlValidationError(
    sshPasswordFormControl: TouchCapableDirtyCapableFormControl,
    sshKeyFormControl: TouchCapableDirtyCapableFormControl): boolean {
    return sshPasswordFormControl.invalid &&
      sshKeyFormControl.invalid &&
      (sshPasswordFormControl.touched || sshKeyFormControl.touched);
  }

  // these 4 gated PR due to code review
  getLastSubmittedFormTypeFromCredential(credential: Credential): CredentialTypes {
    return credential.type;
  }

  lastSubmittedFormTypeIsSSH(lastSubmittedFormType: CredentialTypes) {
    return lastSubmittedFormType === CredentialTypes.SSH;
  }

  lastSubmittedFormTypeIsWinRM(lastSubmittedFormType: CredentialTypes) {
    return lastSubmittedFormType === CredentialTypes.WinRM;
  }

  lastSubmittedFormTypeIsSudo(lastSubmittedFormType: CredentialTypes) {
    return lastSubmittedFormType === CredentialTypes.Sudo;
  }
}
