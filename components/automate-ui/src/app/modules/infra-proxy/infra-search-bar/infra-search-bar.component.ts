import {
  Component,
  EventEmitter,
  Output,
  Input
} from '@angular/core';
// import { Regex } from 'app/helpers/auth/regex';

@Component({
  selector: 'app-infra-search-bar',
  templateUrl: './infra-search-bar.component.html',
  styleUrls: ['./infra-search-bar.component.scss']
})

export class InfraSearchBarComponent {
  inputText = '';
  formActive = false;
  error = false;

  @Input() placeHolder: string;
  @Output() searchButtonClick = new EventEmitter<string>();

  onSubmit(currentText: string): void {
    this.searchButtonClick.emit(currentText);
  }

  toggleFocus(): void {
    this.formActive = !this.formActive;
  }
}
