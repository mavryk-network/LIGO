///////////////////////////////////////////////////////
// Adds "os" field to the npm package		     //
// Usage: node ./npm_update_os_arch.js /path/package.json //
///////////////////////////////////////////////////////

const path = require("path");

let PACKAGE_JSON_FILEPATH = process.argv[1];

function printUsage() {
  console.log(`

  Usage: node npm_update_os_arch.js /path/to/package.json

`);
  process.exit(-1);
}

if (!PACKAGE_JSON_FILEPATH) {
  printUsage();
}

if (/npm_update_os/.test(PACKAGE_JSON_FILEPATH)) {
  if (!process.argv[2]) {
    printUsage();
  } else {
    PACKAGE_JSON_FILEPATH = process.argv[2];
  }
}

PACKAGE_JSON_FILEPATH = path.resolve(PACKAGE_JSON_FILEPATH);
const packageJson = require(PACKAGE_JSON_FILEPATH);

packageJson.os = [process.platform];
packageJson.cpu = [process.arch];
console.log(JSON.stringify(packageJson, null, 2));
