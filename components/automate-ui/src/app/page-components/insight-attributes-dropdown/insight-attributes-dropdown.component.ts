import { Component, OnInit } from '@angular/core';
import { FilterOptions, FilterableOptions } from './insight-attributes-dropdown.model';


@Component({
  selector: 'app-insight-attributes-dropdown',
  templateUrl: './insight-attributes-dropdown.component.html',
  styleUrls: ['./insight-attributes-dropdown.component.scss']
})
export class InsightAttributesDropdownComponent implements OnInit {

  public options: FilterOptions[] = FilterableOptions;

  ngOnInit() {
    console.log('on init');
  }
}
