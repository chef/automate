import {
  Component,
  EventEmitter,
  Output,
  Input
} from '@angular/core';
import { Regex } from 'app/helpers/auth/regex';

@Component({
  selector: 'app-infra-search-bar',
  templateUrl: './infra-search-bar.component.html',
  styleUrls: ['./infra-search-bar.component.scss']
})

export class InfraSearchBarComponent {
  inputText = '';
  formActive = false;
  error = false;
  disabled = false;

  @Input() placeHolder: string;
  @Output() searchButtonClick = new EventEmitter<string>();

  onSubmit(currentText: string): void {
    this.searchButtonClick.emit(currentText);
  }

  toggleFocus(): void {
    this.formActive = !this.formActive;
  }

  handleEditVersion(event: { target: { value: string}}) {
    const search = event.target.value;
    if ( search !== ''  && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(search)) {
      this.error = true;
      this.disabled = true;
    } else {
      this.error = false;
      this.disabled = false;
    }
  }
}
