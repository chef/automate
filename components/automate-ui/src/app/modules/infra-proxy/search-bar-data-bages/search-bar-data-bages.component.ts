import {
  Component,
  EventEmitter,
  Output,
  OnInit,
  ViewChild,
  ElementRef,
} from '@angular/core';

@Component({
  selector: 'app-search-bar-data-bages',
  templateUrl: './search-bar-data-bages.component.html',
  styleUrls: ['./search-bar-data-bages.component.scss']
})

export class SearchBarDataBagesComponent implements OnInit {
  inputText= '';
  
  @Output() searchButtonClick: EventEmitter<any> = new EventEmitter<any>();
  @ViewChild('search_box', { static: true }) inputField: ElementRef;

  ngOnInit() {}

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
    return 'Search data bag Items';
  }

  // clearAll(): void {
  //   this.inputField.nativeElement.value = '';
  // } 
}
