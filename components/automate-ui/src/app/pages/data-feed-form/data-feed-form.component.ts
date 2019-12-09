import { Component } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { Destination } from 'app/pages/data-feed/destination';
import { DatafeedService } from 'app/services/data-feed/data-feed.service';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

enum SaveState {
  Inactive,
  Success,
  Failure
}

class Model {
  constructor(
    public destination: Destination,
    public targetUsername: string,
    public targetPassword: string
  ) {
  }
}

type Modal = 'save' | 'url';

@Component({
  selector: 'app-data-feed-form',
  templateUrl: './data-feed-form.component.html',
  styleUrls: ['./data-feed-form.component.scss']
})
export class DatafeedFormComponent {
  model: Model;
  urlState = UrlTestState; // expose enum to template
  saveState = SaveState; // expose enum to template
  hookStatus = UrlTestState.Inactive;
  saveStatus = SaveState.Inactive;
  alertOptions: Array<object>;
  id: string;
  isEditDestination: boolean;
  showLoading = false;
  urlStatusModalVisible = false;
  saveStatusModalVisible = false;


  constructor(
    private layoutFacade: LayoutFacadeService,
    private datafeedService: DatafeedService,
    private route: ActivatedRoute
  ) {
    this.layoutFacade.showSidebar('settings');
    this.model = new Model(new Destination(undefined, '', '', ''), '', '');
    this.id = this.route.snapshot.params['id'];
    this.isEditDestination = this.id ? true : false;
    this.showLoading = this.isEditDestination;

    if (this.isEditDestination) {
      this.datafeedService.fetchDestination(this.id)
        .subscribe( destination => {
          this.showLoading = false;
          return this.model =
            new Model(new Destination(destination.id, destination.name, destination.targetUrl,
              destination.targetSecretId), '', '');
        });
    }
  }

  private revealSaveStatus(status: SaveState) {
    this.saveStatus = status;
    this.openModal('save');
  }

  private revealUrlStatus(status: UrlTestState) {
    this.hookStatus = status;
    this.openModal('url');
  }

  testHook($event: Event) {
    $event.preventDefault();
    this.hookStatus = UrlTestState.Loading;
    if (this.model.targetUsername) {
      this.datafeedService.testDestinationWithUsernamePassword(this.model.destination.targetUrl,
        this.model.targetUsername, this.model.targetPassword).subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else if (this.model.destination.targetSecretId) {
      this.datafeedService.testDestinationWithSecretId(this.model.destination.targetUrl,
        this.model.destination.targetSecretId)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    } else {
      this.datafeedService.testDestinationWithNoCreds(this.model.destination.targetUrl)
        .subscribe(
          () => this.revealUrlStatus(UrlTestState.Success),
          () => this.revealUrlStatus(UrlTestState.Failure)
        );
    }
  }

  onSubmit() {

    if (this.isEditDestination) {
      this.datafeedService.editDestination(this.model.destination).subscribe(
          () => this.revealSaveStatus(SaveState.Success),
          () => this.revealSaveStatus(SaveState.Failure)
      );
    } else {
      this.datafeedService.createDestination(this.model.destination, this.model.targetUsername,
        this.model.targetPassword).subscribe(
          () => this.revealSaveStatus(SaveState.Success),
          () => this.revealSaveStatus(SaveState.Failure)
      );
    }
  }

  public openModal(type: Modal): void {
    switch (type) {
      case 'save':
        this.saveStatusModalVisible = true;
        return;
      case 'url':
        this.urlStatusModalVisible = true;
        return;
      default:
        return;
    }
  }

  public closeModal(type: Modal): void {
    switch (type) {
      case 'save':
        this.saveStatusModalVisible = false;
        return;
      case 'url':
        this.urlStatusModalVisible = false;
        return;
      default:
        return;
    }
  }
}
