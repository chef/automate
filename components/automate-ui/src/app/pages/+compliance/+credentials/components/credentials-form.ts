import { Component, Output, EventEmitter, Input, OnChanges } from '@angular/core';
import { Credential, CredentialTypes } from '../credentials.state';
import { CredentialsLogic } from '../credentials.logic';
import { FormControl, Validators, FormGroup, FormBuilder } from '@angular/forms';

@Component({
  selector: 'app-credentials-form',
  templateUrl: './credentials-form.html',
  styleUrls: ['./credentials-form.scss']
})

export class CredentialsFormComponent implements OnChanges {
  @Output()
  saveCredential: EventEmitter<Credential> = new EventEmitter<Credential>();
  @Input()
  saveErrorOccurred = false;
  @Input()
  saveErrorMessage = '';
  @Input()
  lastSubmittedFormType: CredentialTypes = CredentialTypes.SSH;
  @Input()
  credential: Credential | null;

  private defaultFormData = {
    name: '',
    type: 'ssh',
    ssh: {
      username: '',
      password: '',
      key: ''
    },
    winrm: {
      username: '',
      password: ''
    },
    sudo: {
      password: '',
      options: ''
    }
  };

  public credTypesOrder: Array<CredentialTypes> = [
    CredentialTypes.SSH,
    CredentialTypes.WinRM,
    CredentialTypes.Sudo
  ];

  // TODO: add form validation on submit later.  Thought we were supposed to do required
  // field validation, but then remembered we are not supposed to do any at all for now.
  // So punting on submit triggering validation.  We agreed to leave the onblur validation in the
  // form, even though it's not blocking submitting.
  sshForm: FormGroup;

  winRMForm: FormGroup;

  sudoForm: FormGroup;

  form: FormGroup;

  constructor(
    public credentialsLogic: CredentialsLogic,
    private fb: FormBuilder
  ) {}

  ngOnChanges(changes) {
    const {credential} = changes;
    if (credential) {
      this.createForm(credential.currentValue || {});
    }
  }

  getSelectedIndex(credType = CredentialTypes.SSH) {
    return this.credTypesOrder.indexOf(credType);
  }

  createForm(cred) {
    let formData = this.defaultFormData;
    if (cred.id) {
      formData = Object.assign(formData, cred);
      cred.data.forEach(d => {
        formData[cred.type][d.key] = d.value;
      });
    }

    this.sshForm = this.fb.group({
      name: [formData.name, Validators.required],
      username: [formData['ssh']['username'], Validators.required],
      password: [formData['ssh']['password'], Validators.required],
      key: [formData['ssh']['key'], Validators.required]
    });

    this.winRMForm = this.fb.group({
      name: [formData.name, Validators.required],
      username: [formData['winrm']['username'], Validators.required],
      password: [formData['winrm']['password'], Validators.required]
    });

    this.sudoForm = this.fb.group({
      name: [formData.name, Validators.required],
      password: [formData['sudo']['password'], Validators.required],
      options: [formData['sudo']['options'], Validators.required]
    });

    this.form = this.fb.group({
      name: [formData['name'], Validators.required],
      type: [formData['type'], Validators.required],
      ssh: this.sshForm,
      winrm: this.winRMForm,
      sudo: this.sudoForm
    });

    if (cred.id) {
      this.form.addControl('id', new FormControl(cred.id));
    }
  }

  handleCredentialTypeChange(event) {
    this.credential.type = this.credTypesOrder[event.target.value];
    this.form.patchValue({type: this.credTypesOrder[event.target.value]});
  }

  handleFormSubmit(data) {
    const credBody = {
      id: data['id'],
      name: data[data['type']]['name'],
      type: data['type'],
      data: [],
      tags: []
    };

    switch (data['type']) {
      case CredentialTypes.SSH: {
        credBody.data.push({key: 'username', value: data['ssh']['username']});
        if (data['ssh']['password'] !== '') {
          credBody.data.push({key: 'password', value: data['ssh']['password']});
        }
        if (data['ssh']['key'] !== '') {
          credBody.data.push({key: 'key', value: data['ssh']['key']});
        }
        break;
      }
      case CredentialTypes.WinRM: {
        credBody.data.push(
          {key: 'username', value: data['winrm']['username']},
          {key: 'password', value: data['winrm']['password']}
        );
        break;
      }
      case CredentialTypes.Sudo: {
        credBody.data.push(
          {key: 'password', value: data['sudo']['password']},
          {key: 'options', value: data['sudo']['options']}
        );
        break;
      }
      default: {
        throw new Error(`Invalid credential type: ${data['type']}`);
      }
    }

    this.saveCredential.emit(credBody);
  }
}
