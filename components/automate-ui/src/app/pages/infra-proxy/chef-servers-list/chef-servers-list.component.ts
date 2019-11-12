import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-chef-servers-list',
  templateUrl: './chef-servers-list.component.html',
  styleUrls: ['./chef-servers-list.component.scss']
})
export class ChefServersListComponent implements OnInit {
  public serverMockList = [
    {
      id: '1',
      name: 'Chef Server',
      fqdn: 'chef.io',
      ip_address: '27.0.0.5',
      orgs: '3'
    },
    {
      id: '2',
      name: 'Test Server For Complience',
      fqdn: 'test.chef.io',
      ip_address: '127.0.0.1',
      orgs: '1'
    }
  ];
  constructor() { }

  ngOnInit() {
  }

}
