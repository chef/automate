import {
  Component,
  Input,
  EventEmitter,
  Output,
  HostBinding
} from '@angular/core';
import { PolicyFile } from 'app/entities/policy-files/policy-file.model';
import { PolicyFileRequests } from 'app/entities/policy-files/policy-file.requests';

@Component({
  selector: 'app-included-policies-details',
  templateUrl: './included-policies-details.component.html',
  styleUrls: ['./included-policies-details.component.scss']
})

export class IncludedPoliciesDetailsComponent {
  @Input() serverId: string;
  @Input() orgId: string;
  @Output() resetKeyRedirection = new EventEmitter<boolean>();

  public error = false;
  public policyfileName: string;
  public Policyfile: PolicyFile;
  public policyfileLoading = true;
  public revisionId: string;
  @HostBinding('class.active') isSlideOpen = false;

  constructor(
    private policyFileRequests: PolicyFileRequests
  ) { }

  closeIncludedPolicyDetails() {
    this.toggleSlide();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel(policyfile: string, revision: string) {
    this.policyfileName = policyfile;
    this.revisionId = revision;
    this.isSlideOpen = true;
    this.getPolicyfileDetails(this.policyfileName, this.revisionId);
  }

  policyfiledetails() {
    this.isSlideOpen = false;
  }

  private getPolicyfileDetails(policyfileName, revisionId): void {
    this.policyFileRequests.getPolicyFile(this.serverId, this.orgId, policyfileName, revisionId)
    .pipe().subscribe(
      (success) => {
        this.Policyfile = success;
        this.policyfileLoading = false;
        this.error = false;
      },
      () => {
        this.policyfileLoading = false;
        this.error = true;
      }
    );
  }

}
