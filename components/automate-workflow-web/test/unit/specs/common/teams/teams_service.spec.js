import ng from 'angular';
import 'angular-mocks';
import teamsService from '../../../../../src/common/teams/teams_service';

describe('TeamsService', () => {
  let TeamsService, httpBackend;

  beforeEach(ng.mock.module(teamsService, ($provide) => {
    $provide.value('ApiUrl', () => '/teams');
  }));

  beforeEach(inject((_TeamsService_, $httpBackend) => {
    TeamsService = _TeamsService_;
    httpBackend = $httpBackend;
  }));

  describe('in its initial state', () => {

    describe('the teams property', () => {

      it('is an empty array', () => {
        expect(TeamsService.teams).toEqual([]);
      });
    });
  });

  describe('fetch', () => {

    describe('when successful', () => {

      beforeEach(() => {
        httpBackend
          .when('GET', '/teams')
          .respond({
            teams: [
              'the-mariners',
              'the-angels'
            ]
          });

        TeamsService.fetch();
        httpBackend.flush();
      });

      it('sets the teams array', () => {
        expect(TeamsService.teams).toEqual([
          'the-mariners',
          'the-angels'
        ]);
      });
    });

    describe('when unsuccessful', () => {

      beforeEach(() => {
        httpBackend
          .when('GET', '/teams')
          .respond(500);

        TeamsService.fetch();
        httpBackend.flush();
      });

      it('does not set the teams array', () => {
        expect(TeamsService.teams).toEqual([]);
      });
    });
  });

  describe('create', () => {

    describe('when successful', () => {

      beforeEach(() => {
        httpBackend
          .when('POST', '/teams')
          .respond({
            teams: [
              'the-mariners',
              'the-angels'
            ]
          });

        spyOn(TeamsService, 'fetch');

        TeamsService.create({
          name: 'the-giants',
          description: 'woo!'
        });

        httpBackend.flush();
      });

      it('calls fetch', () => {
        expect(TeamsService.fetch).toHaveBeenCalled();
      });
    });
  });
});
