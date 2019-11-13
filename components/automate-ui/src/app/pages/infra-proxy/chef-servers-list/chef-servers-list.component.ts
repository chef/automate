import { Component, OnInit, EventEmitter } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-chef-servers-list',
  templateUrl: './chef-servers-list.component.html',
  styleUrls: ['./chef-servers-list.component.scss']
})
export class ChefServersListComponent implements OnInit {
  public createModalVisible = false;
  public createChefServerForm: FormGroup;
  public creatingChefServer = false;
  public conflictErrorEvent = new EventEmitter<boolean>();

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

  public createChefServer(): void {
  }

  public openCreateModal(): void {
    this.createModalVisible = true;
  }

  public closeCreateModal(): void {
    this.createModalVisible = false;
  }
}
