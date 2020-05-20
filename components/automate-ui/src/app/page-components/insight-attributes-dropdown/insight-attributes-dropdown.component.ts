import { Component, OnInit } from '@angular/core';
import { FilterOption, FilterableOptions } from './insight-attributes-dropdown.model';


@Component({
  selector: 'app-insight-attributes-dropdown',
  templateUrl: './insight-attributes-dropdown.component.html',
  styleUrls: ['./insight-attributes-dropdown.component.scss']
})
export class InsightAttributesDropdownComponent implements OnInit {

  public options: FilterOption[] = FilterableOptions;

  ngOnInit() {
    console.log('on init');
  }

  public handleSelect(event: Event) {
    const target = event.target as HTMLElement;
    const filter = target.getAttribute('data-filterValue');
    console.log(filter);
  }
}
