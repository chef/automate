import { E2EElement, E2EPage, newE2EPage } from '@stencil/core/testing';

describe('chef-badge', () => {
  let page: E2EPage;
  let element: E2EElement;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`
      <chef-badge>Badge Text</chef-badge>
    `);
    element = await page.find('chef-badge');
  });

  it('renders', async () => {
    expect(element).toHaveClass('hydrated');
  });

  describe('when `no-data` prop is not set', () => {
    it('defaults `no-data` attr to `false`', async () => {
      expect(element.getAttribute('noData')).toBeNull();
    });
  });

  describe('when `no-data` property is true', () => {
    beforeEach(async () => {
      element.setProperty('noData', true);
      await page.waitForChanges();
    });

    it('has an `no-data` attribute', async () => {
      expect(element.getAttribute('noData')).toBeNull();
    });
  });

  describe('when `id` property is set', () => {
    beforeEach(async () => {
      element.setProperty('id', 'myId');
      await page.waitForChanges();
    });

    it('has an `id` attribute', async () => {
      expect(element.getAttribute('id')).toEqual('myId');
    });
  });

  describe('when `id` property is not set', () => {

    it('will not have a `id` attribute', async () => {
      expect(element.getAttribute('id')).toBeNull();
    });
  });

  describe('when `tooltip` property is set', () => {
    beforeEach(async () => {
      element.setProperty('id', 'myId');
      element.setProperty('tooltip', 'Tooltip');
      await page.waitForChanges();
    });

    it('has an `tooltip` attribute', async () => {
      expect(element.getAttribute('tooltip')).toEqual('Tooltip');
    });
  });

  describe('when `tooltip` property is not set', () => {

    it('will not have a `tooltip` attribute', async () => {
      expect(element.getAttribute('tooltip')).toBeNull();
    });
  });
});
