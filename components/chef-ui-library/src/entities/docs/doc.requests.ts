export const fetchDocs = () => {
  const request = new Request('/assets/docs.json');

  return fetch(request);
};
