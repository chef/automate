import { FormControl, FormGroup } from '@angular/forms';

// Custom validators for use on Angular Forms.
export class ChefValidators {

  // Typical use of matchFieldValidator is confirming a doubly-entered password
  public static matchFieldValidator(otherField: string) {
    return (control: FormControl): { [key: string]: any } => {
      if (!control.root || !(<FormGroup>control.root).controls) {
        return null;
      }
      const valid = control.value === (<FormGroup>control.root).controls[otherField].value;
      return valid ? null : {
        'noMatch': { value: control }
      };
    };
  }

  // Validates length but only for non-admin users; admins are exempt from the check.
  public static nonAdminLengthValidator(isAdminView: boolean, minLength: number) {
    return (control: FormControl): { [key: string]: any } => {
      // Don't error if we are in admin view.
      if (isAdminView) {
        return null;
      }
      if (!control.value) {
        return { 'required': { value: control } };
      }
      if (control.value.length < minLength) {
        return { 'minlength': { value: control } };
      }
      return null;
    };
  }
}
