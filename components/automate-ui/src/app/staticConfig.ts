interface StaticConfig {
  products: string[];
}
const staticConfig: StaticConfig = window['staticAutomateConfig'] || {};

export function isProductDeployed(productName: string): boolean {
  return staticConfig.products.includes(productName);
}
