import { E2EElement, E2EPage, newE2EPage } from '@stencil/core/testing';

describe('chef-progress-bar', () => {
  let page: E2EPage;
  let element: E2EElement;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`
      <chef-progress-bar></chef-progress-bar>
    `);
    element = await page.find('chef-progress-bar');
  });

  it('renders', async () => {
    expect(element).toHaveClass('hydrated');
  });

  it('sets `role` attribute to `progressbar`', async () => {
    expect(element.getAttribute('role')).toEqual('progressbar');
  });

  it('sets `aria-valuenow` attribute', async () => {
    expect(element.getAttribute('aria-valuenow')).toEqual('0');
  });

  it('sets `aria-valuemin` attribute', async () => {
    expect(element.getAttribute('aria-valuemin')).toEqual('0');
  });

  it('sets `aria-valuemax` attribute', async () => {
    expect(element.getAttribute('aria-valuemax')).toEqual('100');
  });

  it('displays a progress bar based on value, valueMin, and valueMax', async () => {
    element.setProperty('value', 0.5);
    element.setProperty('valueMin', 0);
    element.setProperty('valueMax', 1);
    await page.waitForChanges();

    const progressBars = element.querySelector('.progress-bars');
    const valueBar = progressBars.querySelector('.value');
    expect(valueBar.getAttribute('style')).toContain('width: 50%');
  });

  describe('when prefixText or suffixText is set', () => {
    beforeEach(async () => {
      element.setProperty('prefixText', 'Uploading files');
      element.setProperty('suffixText', '10% completed');
      await page.waitForChanges();
    });

    it('sets `aria-valuetext` attribute', async () => {
      expect(element.getAttribute('aria-valuetext')).toEqual('Uploading files 10% completed');
    });

    it('displays progress text', async () => {
      const progressText = element.querySelector('.progress-text');
      const prefixText = progressText.querySelector('.prefix');
      const suffixText = progressText.querySelector('.suffix');
      expect(progressText).not.toBeNull();
      expect(prefixText.textContent).toContain('Uploading files');
      expect(suffixText.textContent).toContain('10% completed');
    });
  });

  describe('when prefixText and suffixText is not set', () => {
    beforeEach(async () => {
      element.setProperty('prefixText', '');
      element.setProperty('suffixText', '');
      await page.waitForChanges();
    });

    it('does not set `aria-valuetext` attribute', () => {
      expect(element.hasAttribute('aria-valuetext')).toEqual(false);
    });

    it('does not display progress text', () => {
      const progressText = element.querySelector('.progress-text');
      expect(progressText).toBeNull();
    });
  });
});
