
describe('Available profile', () => {
    it('can be installed', () => {
        const cypressPrefix = 'test-install-available-profile';
        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: 'api/v0/compliance/profiles?owner=admin',
            body: {
            name: 'linux-baseline',
            version: '2.2.2'
            }
        }).then((resp: Cypress.ObjectLike) => {
            expect(resp.body.summary.valid).to.equal(true);
        });
    });
    it('can be installed even when lax header present', () => {
        const cypressPrefix = 'test-install-available-profile';
        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN'), 'content-type': 'application/json+lax' },
            method: 'POST',
            url: 'api/v0/compliance/profiles?owner=admin',
            body: {
            name: 'linux-baseline',
            version: '2.2.2'
            }
        }).then((resp: Cypress.ObjectLike) => {
            expect(resp.body.summary.valid).to.equal(true);
        });
    });
});