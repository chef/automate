import { Component } from '@angular/core';
import { ProjectsFilterService } from 'app/services/projects-filter/projects-filter.service';

@Component({
  selector: 'app-projects-filter',
  templateUrl: './projects-filter.component.html',
  styleUrls: [ './projects-filter.component.scss' ]
})
export class ProjectsFilterComponent {
  constructor(public projectsFilter: ProjectsFilterService) {
    this.projectsFilter.loadOptions();
  }
}
