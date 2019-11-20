import { Component, OnInit } from '@angular/core';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';

@Component({
  selector: 'app-applications-dashboard',
  templateUrl: './applications.component.html',
  styleUrls: ['./applications.component.scss']
})

export class ApplicationsComponent implements OnInit {

  constructor(
    private layoutFacade: LayoutFacadeService
  ) {}

  ngOnInit() {
    this.layoutFacade.showApplicationsSidebar();
  }
}
