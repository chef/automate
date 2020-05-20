import { Component, OnInit } from '@angular/core';
import { FilterOption, FilterableOptions } from './insight-attributes-dropdown.model';


@Component({
  selector: 'app-insight-attributes-dropdown',
  templateUrl: './insight-attributes-dropdown.component.html',
  styleUrls: ['./insight-attributes-dropdown.component.scss']
})
export class InsightAttributesDropdownComponent implements OnInit {

  public options: FilterOption[] = FilterableOptions;
  public selectedOptions: string[] = []; // these are filter ids

  ngOnInit() {
    console.log('on init');
  }

  public handleSelect(event: Event) {
    const target = event.target as HTMLElement;
    const filter = target.getAttribute('data-filterValue');

    const isFilteredIndex = this.selectedOptions.indexOf(filter);
    if ( isFilteredIndex >= 0 ) {
        this.selectedOptions.splice(isFilteredIndex, 1);
        target.classList.remove('selected');
    } else {
      if (this.selectedOptions.length < 5) {
      this.selectedOptions.push(filter);
      target.classList.add('selected');
      }
    }
  }

}
