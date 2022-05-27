import {
  Component,
  Input,
  EventEmitter,
  Output,
  HostBinding
} from '@angular/core';
import {
  CookbookRuleList,
  CookbookDependencyList,
  CookbookList
} from '../policy-file-details/policy-file-details.component';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-cookbook-dependencies',
  templateUrl: './cookbook-dependencies.component.html',
  styleUrls: ['./cookbook-dependencies.component.scss']
})

export class CookbookDependenciesComponent {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  @HostBinding('class.active') isSlideOpen = false;

  public activeDependenciesRules: string;
  public activeCookbooks: string;
  public showDependenciesRules = false;
  public showCookbooks = false;
  public cookbooks: CookbookList[] = [];
  public cookbookRules: CookbookRuleList[] = [];
  public cookbookDependencies: CookbookDependencyList[] = [];

  constructor(private telemetryService: TelemetryService) { }

  closeCookbookDependencies() {
    this.toggleSlide();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel(cookbookRuleList, cookbookDependencyList, cookbookList) {
    this.cookbooks = cookbookList;
    this.cookbookRules = cookbookRuleList;
    this.cookbookDependencies = cookbookDependencyList;
    this.isSlideOpen = true;
    this.telemetryService.track('InfraServer_PolicyFiles_CookbookDependencies');
  }

  handleDependenciesRules() {
    if (!this.showDependenciesRules) {
      this.showDependenciesRules = true;
      this.activeDependenciesRules = 'autoHeight';
    } else {
      this.showDependenciesRules = false;
      this.activeDependenciesRules = '';
    }
  }

  handleCookbooks() {
    if (!this.showCookbooks) {
      this.showCookbooks = true;
      this.activeCookbooks = 'autoHeight';

    } else {
      this.showCookbooks = false;
      this.activeCookbooks = '';
    }
  }

}
