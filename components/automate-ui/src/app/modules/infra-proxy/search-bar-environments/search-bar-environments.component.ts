import {
  Component,
  EventEmitter,
  Output,
  OnInit,
  ViewChild,
  ElementRef,
} from '@angular/core';

@Component({
  selector: 'app-search-bar-environments',
  templateUrl: './search-bar-environments.component.html',
  styleUrls: ['./search-bar-environments.component.scss']
})
export class SearchBarEnvironmentsComponent implements OnInit {
  inputText= '';

  @Output() searchButtonClick: EventEmitter<any> = new EventEmitter<any>();
  @ViewChild('search_box', { static: true }) inputField: ElementRef;

  ngOnInit(): void {
  }

  handleFiltersClick(currentText: string): void {
    console.log(currentText);
    this.searchButtonClick.emit(currentText);    
  }

  pressEnter(currentText: string): void {
    console.log(currentText);
    this.searchButtonClick.emit(currentText);
  }

  handleInput(key, currentText): void {
    switch (key.toLowerCase()) {
      case 'enter':
        this.pressEnter(currentText);
        break;
    }
  }

  getFilterText(): string {
    return 'Search Environments';
  }
}
