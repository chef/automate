import { CredentialsLogic } from './credentials.logic';
import { TouchCapableDirtyCapableFormControl,
         buildCredential,
         CredentialTypes } from './credentials.state';

class FakeFormControl implements TouchCapableDirtyCapableFormControl {
  constructor(private invalidInitialState: boolean, private touchedInitialState: boolean) {
  }

  get invalid(): boolean {
    return this.invalidInitialState;
  }

  get touched(): boolean {
    return this.touchedInitialState;
  }
}

describe('CredentialsLogic', () => {
  let credentialsLogic = null;

  beforeEach(() => {
    credentialsLogic = new CredentialsLogic();
  });

  describe('getCssClassFromCredentialType', () => {
    it('returns fa:fa-lock when type is SSH', () => {
      expect(credentialsLogic
        .getCssClassFromCredentialType(CredentialTypes.SSH)).toEqual('fa:fa-terminal');
    });

    it('returns fa:fa-windows when type is WinRM', () => {
      expect(credentialsLogic
        .getCssClassFromCredentialType(CredentialTypes.WinRM)).toEqual('fa:fa-windows');
    });

    it('returns fa:fa-level-up when type is Sudo', () => {
      expect(credentialsLogic
        .getCssClassFromCredentialType(CredentialTypes.Sudo)).toEqual('fa:fa-level-up');
    });

    it('returns fa:fa-lock when type is neither SSH or WinRM', () => {
      expect(credentialsLogic
        .getCssClassFromCredentialType(CredentialTypes.None)).toEqual('fa:fa-lock');
    });
  });

  describe('formatKeyType', () => {
    it('returns SSH when type is SSH', () => {
      expect(credentialsLogic.formatKeyType(CredentialTypes.SSH)).toEqual('SSH');
    });

    it('returns WinRM when type is WinRM', () => {
      expect(credentialsLogic.formatKeyType(CredentialTypes.WinRM)).toEqual('WinRM');
    });

    it('returns Sudo when type is Sudo', () => {
      expect(credentialsLogic.formatKeyType(CredentialTypes.Sudo)).toEqual('Sudo');
    });

    it('returns SSH when type is neither SSH, Sudo or WinRM', () => {
      expect(credentialsLogic.formatKeyType(CredentialTypes.None)).toEqual('SSH');
    });
  });

  describe('showFormControlValidationError', () => {
    it('returns false if invalid false and touched false', () => {
      const fakeFormControl = new FakeFormControl(false, false);
      expect(credentialsLogic.showFormControlValidationError(fakeFormControl)).toEqual(false);
    });

    it('returns false if invalid true and touched false', () => {
      const fakeFormControl = new FakeFormControl(true, false);
      expect(credentialsLogic.showFormControlValidationError(fakeFormControl)).toEqual(false);
    });

    it('returns false if invalid false and touched true', () => {
      const fakeFormControl = new FakeFormControl(false, true);
      expect(credentialsLogic.showFormControlValidationError(fakeFormControl)).toEqual(false);
    });

    it('returns true if invalid true and touched true', () => {
      const fakeFormControl = new FakeFormControl(true, true);
      expect(credentialsLogic.showFormControlValidationError(fakeFormControl)).toEqual(true);
    });
  });

  // return true if they are both invalid and either are touched...
  // return false for every other combination
  describe('showSSHPasswordOrSSHPublicKeyFormControlValidationError', () => {
    it('returns true if password invalid true, password touched true, ' +
      'key invalid true, and key touched false', () => {
      const fakePasswordFormControl = new FakeFormControl(true, true);
      const fakeKeyFormControl = new FakeFormControl(true, false);
      expect(credentialsLogic.showSSHPasswordOrSSHPublicKeyFormControlValidationError(
        fakePasswordFormControl, fakeKeyFormControl)).toEqual(true);
    });

    it('returns true if password invalid true, password touched false, ' +
      'key invalid true, and key touched true', () => {
      const fakePasswordFormControl = new FakeFormControl(true, false);
      const fakeKeyFormControl = new FakeFormControl(true, true);
      expect(credentialsLogic.showSSHPasswordOrSSHPublicKeyFormControlValidationError(
        fakePasswordFormControl, fakeKeyFormControl)).toEqual(true);
    });

    it('returns true if password invalid true, password touched true, ' +
      'key invalid true, and key touched true', () => {
      const fakePasswordFormControl = new FakeFormControl(true, true);
      const fakeKeyFormControl = new FakeFormControl(true, true);
      expect(credentialsLogic.showSSHPasswordOrSSHPublicKeyFormControlValidationError(
        fakePasswordFormControl, fakeKeyFormControl)).toEqual(true);
    });

    it('returns false if password invalid false, password touched false, ' +
      'key invalid false, and key touched false', () => {
      const fakePasswordFormControl = new FakeFormControl(false, false);
      const fakeKeyFormControl = new FakeFormControl(false, false);
      expect(credentialsLogic.showSSHPasswordOrSSHPublicKeyFormControlValidationError(
        fakePasswordFormControl, fakeKeyFormControl)).toEqual(false);
    });

    it('returns false if password invalid true, password touched false, ' +
      'key invalid false, and key touched false', () => {
      const fakePasswordFormControl = new FakeFormControl(true, false);
      const fakeKeyFormControl = new FakeFormControl(false, false);
      expect(credentialsLogic.showSSHPasswordOrSSHPublicKeyFormControlValidationError(
        fakePasswordFormControl, fakeKeyFormControl)).toEqual(false);
    });

    it('returns false if password invalid false, password touched false, ' +
      'key invalid true, and key touched false', () => {
      const fakePasswordFormControl = new FakeFormControl(false, false);
      const fakeKeyFormControl = new FakeFormControl(true, false);
      expect(credentialsLogic.showSSHPasswordOrSSHPublicKeyFormControlValidationError(
        fakePasswordFormControl, fakeKeyFormControl)).toEqual(false);
    });
  });

  describe('lastSubmittedFormTypeIsSSH', () => {
    it('returns true if the credential type is SSH', () => {
      const credential = buildCredential('SSH credential', CredentialTypes.SSH, 'username');
      const lastSubmittedFormType =
        credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);
      expect(credentialsLogic.lastSubmittedFormTypeIsSSH(lastSubmittedFormType)).toEqual(true);
    });

    it('returns false if the credential type is not SSH', () => {
      const credential = buildCredential('ssh credential', CredentialTypes.None, 'username');
      const lastSubmittedFormType =
        credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);
      expect(credentialsLogic.lastSubmittedFormTypeIsSSH(lastSubmittedFormType)).toEqual(false);
    });
  });

  describe('lastSubmittedFormTypeIsWinRM', () => {
    it('returns true if the credential type is WinRM', () => {
      const credential = buildCredential('winrm credential', CredentialTypes.WinRM, 'username');
      const lastSubmittedFormType =
        credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);
      expect(credentialsLogic.lastSubmittedFormTypeIsWinRM(lastSubmittedFormType)).toEqual(true);
    });

    it('returns false if the credential type is not WinRM', () => {
      const credential = buildCredential('ssdfsdfsdfsdfsdfdfsdsh credential',
        CredentialTypes.None, 'username');
      const lastSubmittedFormType =
        credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);
      expect(credentialsLogic.lastSubmittedFormTypeIsWinRM(lastSubmittedFormType)).toEqual(false);
    });
  });

  describe('lastSubmittedFormTypeIsSudo', () => {
    it('returns true if the credential type is Sudo', () => {
      const credential = buildCredential('Sudo credential', CredentialTypes.Sudo, 'username');
      const lastSubmittedFormType =
        credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);
      expect(credentialsLogic.lastSubmittedFormTypeIsSudo(lastSubmittedFormType)).toEqual(true);
    });

    it('returns false if the credential type is not Sudo', () => {
      const credential = buildCredential('ssdfsdfsdfsdfsdfdfsdsh credential',
        CredentialTypes.None, 'username');
      const lastSubmittedFormType =
        credentialsLogic.getLastSubmittedFormTypeFromCredential(credential);
      expect(credentialsLogic.lastSubmittedFormTypeIsSudo(lastSubmittedFormType)).toEqual(false);
    });
  });
});
