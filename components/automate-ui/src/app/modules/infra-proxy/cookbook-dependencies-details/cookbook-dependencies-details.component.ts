import {
  Component,
  Input,
  EventEmitter,
  Output,
  HostBinding
} from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { first } from 'rxjs/operators';
import { environment as env } from 'environments/environment';
import {
  CookbookDetails,
  RootFiles
} from 'app/entities/cookbooks/cookbook-details.model';
import { CookbookDetailsRequests } from 'app/entities/cookbooks/cookbook-details.requests';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

@Component({
  selector: 'app-cookbook-dependencies-details',
  templateUrl: './cookbook-dependencies-details.component.html',
  styleUrls: ['./cookbook-dependencies-details.component.scss']
})

export class CookbookDependenciesDetailsComponent {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  @HostBinding('class.active') isSlideOpen = false;

  public activeDependenciesRules: string;
  public activeCookbooks: string;
  public cookbookName: string;
  public cookbookVersion: string;
  public cookbookDetails: CookbookDetails;
  public readFile: RootFiles;
  public readFileUrl: string;
  public readFileContent;
  public error = false;
  public cookbookDetailsLoading = true;

  public showDependenciesRules = false;
  public showCookbooks = false;

  constructor(
    private http: HttpClient,
    private cookbookDetailsRequests: CookbookDetailsRequests,
    private telemetryService: TelemetryService
  ) { }

  closeCookbookDependencies() {
    this.toggleSlide();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel(cookbook: string, version: string) {
    this.cookbookName = cookbook;
    this.cookbookVersion = version;
    this.loadCookbookDetails();
    this.isSlideOpen = true;
    this.telemetryService.track('InfraServer_PolicyFiles_CookbookDependency_Details');
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

  loadCookbookDetails() {
    this.cookbookDetailsRequests
    .getCookbookDetails(this.serverId, this.orgId, this.cookbookName, this.cookbookVersion)
    .pipe().subscribe(
      (success) => {
        this.cookbookDetails = success;
        this.readFile = success?.root_files.find(data => data.name === 'README.md');
        if (this.readFile) {
          this.readFileUrl = encodeURIComponent(this.readFile?.url);
          this.http.get(
            `${env.infra_proxy_url}/servers/${this.serverId}/orgs/${this.orgId}/cookbooks/${this.cookbookName}/${this.cookbookVersion}/file-content?url=${this.readFileUrl}`)
            .pipe(first())
            .subscribe
            (fileContent => {
              this.readFileContent = fileContent;
            });
        }
        this.cookbookDetailsLoading = false;
        this.error = false;
      },
      () => {
        this.readFileContent = '';
        this.cookbookDetailsLoading = false;
        this.error = true;
      }
    );
  }
}
