import ng from 'angular';
import 'angular-mocks';
import Project from '../../../../../src/common/models/project';
import Organization from '../../../../../src/common/models/organization';
import projectsService from '../../../../../src/common/projects/projects';

describe('Projects', () => {

  beforeEach(ng.mock.module(Project));
  beforeEach(ng.mock.module(Organization));
  beforeEach(ng.mock.module(projectsService));

  describe('create()', () => {

    describe('when creating a local scm project', () => {

      it('should POST to "/projects"', inject(($httpBackend, Projects) => {
        let params = {
          name: 'FooProj',
          scm: {
            type: 'local',
            projectCreateUri: '/projects'
          }
        };

        $httpBackend
          .expectPOST('/projects', params)
          .respond(201);

        Projects.create(params);

        $httpBackend.flush();
      }));
    });
  });

  describe('update()', () => {

    it('should PUT to "/projects/:name"', inject(($httpBackend, Projects, Project, Organization) => {
      let org = Organization.$buildRaw({ name: 'OrgName' });

      let project = org.projects.$buildRaw({
        name: 'BarProj',
        scm: {
          type: 'bitbucket'
        }
      });

      let params = {
        name: 'BarProj',
        scm: {
          type: 'local'
        }
      };

      $httpBackend
        .expectPUT(project.$url(), params)
        .respond(204);

      Projects.update(project, params);

      $httpBackend.flush();
    }));
  });
});
