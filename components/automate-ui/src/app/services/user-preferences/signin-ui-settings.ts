export interface SigninUiSetting {
    isResetPasswordTabVisible: boolean;
    isDisplayNameEditable: boolean;
    isProfileMenu: boolean;
    isTimeformatExist: boolean;
    userType: string;
}

class LocalUser implements SigninUiSetting {
  isResetPasswordTabVisible = true;
  isDisplayNameEditable = true;
  isProfileMenu = true;
  isTimeformatExist = true;
  userType = 'local';
}

class SamlUser implements SigninUiSetting {
  isResetPasswordTabVisible = false;
  isDisplayNameEditable = false;
  isProfileMenu = true;
  isTimeformatExist = true;
  userType = 'saml';
}

class LdapUser implements SigninUiSetting {
  isResetPasswordTabVisible = false;
  isDisplayNameEditable = false;
  isProfileMenu = true;
  isTimeformatExist = true;
  userType = 'ldap';
}

export class UISettings {
  local: SigninUiSetting = new LocalUser();
  saml: SigninUiSetting = new SamlUser();
  ldap: SigninUiSetting = new LdapUser();
}
