import http from 'k6/http';
const url = 'https://a2-dev.test/api/v0/cfgmgmt/clientrun';
export default function () {
  let data = {
    offset: 0,
    size: 40,
    feedStart: "2020-03-01T15:04:05Z",
    feedEnd: "2021-04-01T15:04:05Z",
    attribute: [
        "id"
    ]
};
  // Using a JSON string as body
  let res = http.post(url, JSON.stringify(data),
                      { headers: { 'Content-Type': 'application/json' } });
  console.log(res.json().json.name); // Bert
  // Using an object as body, the headers will automatically include
  // 'Content-Type: application/x-www-form-urlencoded'.
  res = http.post(url, data);
  console.log(res.json().form.name); // Bert
  // Using a binary array as body. Make sure to open() the file as binary
  // (with the 'b' argument).
//   http.post(url, logoBin, { headers: { 'Content-Type': 'image/png' }});
  // Using an ArrayBuffer as body. Make sure to pass the underlying ArrayBuffer
  // instance to http.post(), and not the TypedArray view.
  data = new Uint8Array([104, 101, 108, 108, 111]);
//   http.post(url, data.buffer, { headers: { 'Content-Type': 'image/png' }});
}