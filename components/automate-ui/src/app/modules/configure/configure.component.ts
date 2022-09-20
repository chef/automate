import { Component, OnInit } from '@angular/core';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ConfigService } from 'app/services/config/config.service';
@Component({
  selector: 'app-configure',
  templateUrl: './configure.component.html',
  styleUrls: ['./configure.component.scss']
})
export class ConfigureComponent implements OnInit {

  isNotificationVisible = false;

  constructor(
    private layoutFacade: LayoutFacadeService,
    private configService: ConfigService
  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.isNotificationVisible = false;
    this.getConfigDetails();
  }

  getConfigDetails() {
    this.configService.getConfig().subscribe(
      config => {
        if (config.deploymentType !== 'SAAS') {
          this.isNotificationVisible = true;
        }
      }
    );
  }

  hideNotification() {
    this.isNotificationVisible = false;
  }
}
