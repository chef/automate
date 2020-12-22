import {
  AbstractControl,
  FormArray,
  ValidationErrors,
  ValidatorFn
} from '@angular/forms';

export const nodeSelectionRequiredValidator: ValidatorFn =
  (control: AbstractControl): ValidationErrors | null => {
  const managers = control.get('managers') as FormArray;
  const hasSelected = managers.controls.some(c => c.get('include').value);
  return hasSelected ? null : { 'nodeSelectionRequired': true };
};

export const profileSelectionRequiredValidator: ValidatorFn =
  (control: AbstractControl): ValidationErrors | null => {
  const profiles = control.get('profiles') as FormArray;
  const hasSelected = profiles.controls.some(c => c.get('include').value);
  return hasSelected ? null : { 'profileSelectionRequired': true };
};
